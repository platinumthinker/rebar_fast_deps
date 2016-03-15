-module(deps).

-export([
         foreach/4,
         foreach/5,
         foreach/6
        ]).

-include("rebar.hrl").

-define(ROOT, bfs_root).
-callback do(Dir :: string(), App :: atom(), _VSN, _Source, _Args) ->
    {nothing, App :: atom()} |
    {ok, App :: atom(), Output :: string()} |
    {accum, App :: atom(), Res :: any()} |
    {error, App :: atom(), Reason :: string()}.

-spec foreach(Dir :: string(), Module :: module(),
              Acc :: any(), Args :: any()) ->
    {ok, Res :: any()} | {error, Reason :: any()}.
foreach(Dir, Module, Acc, Args) ->
    foreach(Dir, Module, Acc, Args, false, ?REBAR_CFG).
foreach(Dir, Module, Acc, Args, RebarCfg) ->
    foreach(Dir, Module, Acc, Args, false, RebarCfg).

foreach(Dir, Module, Acc, Args, Delay, RebarCfg) ->
    ok = file:set_cwd(Dir),
    {DepsFldr, DepsList} = case file:consult(RebarCfg) of
        {ok, Config} ->
            {
             proplists:get_value(deps_dir, Config, "deps"),
             proplists:get_value(deps, Config, [])
            };
        {error, enoent} -> {"deps", []}
    end,
    DepsFolder = filename:join(Dir, DepsFldr),
    bfs_step(Module, DepsFolder, DepsList, Acc, Delay, Args).

-spec bfs_step(Module :: module(), Dir :: string(),
               DownloadList :: list({_, _, _, _} | {_, _, _} | {_, _}),
               AccResult :: any(), Delay :: boolean(),
			   Args :: any()) -> {ok, list()} | none.
bfs_step(Module, Dir, DepsList, AccResult, Delay, Args) ->
    {Q, ViewedDeps} = lists:foldl(
      fun({App, VSN, Source, [raw]}, {Acc1, Acc2}) ->
              {
               queue:in({App, VSN, Source}, Acc1),
               gb_sets:add(App, Acc2)
              };
         (A = {App, _, _}, {Acc1, Acc2}) ->
              {
               queue:in(A, Acc1),
               gb_sets:add(App, Acc2)
              };
         (Drop, Acc) ->
              ?WARN("Drop ~p", [Drop]),
              Acc
      end, {queue:new(), gb_sets:new()}, DepsList),

    case lists:member(?ROOT, erlang:registered()) of
        true ->
            none;
        false ->
            true = erlang:register(?ROOT, self())
    end,
    bfs_step(Module, Dir, Q, ViewedDeps, DepsList, gb_sets:new(),
             AccResult, Delay, Args).

bfs_step(Module, Dir, Queue, ViewedDeps, DownloadList, DownloadedList,
         AccResult, Delay, Args) ->
    CorrectDownList = lists:reverse(lists:foldl(
      fun(A = {App, VSN, Source}, Acc) ->
               spawn(
                 fun() ->
                        Delay andalso timer:sleep(100),
                        ?ROOT ! Module:do(Dir, App, VSN, Source, Args)
                 end),
              [A | Acc];
         ({App, VSN, Source, [raw]}, Acc) ->
               spawn(
                 fun() ->
                        Delay andalso timer:sleep(100),
                        ?ROOT ! Module:do(Dir, App, VSN, Source, Args)
                 end),
              [{App, VSN, Source} | Acc];
         (Drop, Acc) ->
              ?WARN("Drop ~p", [Drop]),
              Acc
      end, [], DownloadList)),

    {DownL, NewAccResult} = lists:foldl(
      fun({_, _, _}, {Acc, AccRes}) ->
              receive
                  {nothing, App} ->
                      {gb_sets:add(App, Acc), AccRes};
                  {ok, App, Output} ->
                      ?CONSOLE(Output, []),
                      {gb_sets:add(App, Acc), AccRes};
                  {accum, App, Result} when not is_list(AccRes) ->
                      {
                       gb_sets:add(App, Acc),
                       Result
                      };
                  {accum, App, Result} ->
                      {
                       gb_sets:add(App, Acc),
                       lists:flatten([Result | AccRes])
                      };
                  {error, App, Reason} ->
                      ?ERROR("\e[1m\e[31m~p\e[0m: ~n~p", [App, Reason]),
                      exit("Error when deps update")
              after ?TIMEOUT ->
                    exit("Timeout when update dep")
              end;
         (_, Acc) -> Acc
      end, {DownloadedList, AccResult}, CorrectDownList),

    Size1 = gb_sets:size(DownL),
    Size2 = gb_sets:size(DownloadedList),
    (Size1 - 5 > Size2) andalso Delay
        andalso ?CONSOLE("Prepare ~p deps", [Size1]),

    case queue:out(Queue) of
        {{value, {App, _Vsn, _Source}}, Q} ->
            AppDir = filename:join([Dir, App, ?REBAR_CFG]),
            Child = try
                case file:consult(AppDir) of
                    {ok,    Config} -> proplists:get_value(deps, Config, []);
                    {error, enoent} -> []
                end
            catch
                _:E ->
                    ?ERROR("Error when file ~s consult: ~p", [AppDir, E]),
                    throw(E)
            end,


            {NewQ, NewS, DownloadL} = lists:foldl(
                     fun(Item = {Dep, _, _}, {AccQ, AccS, AccD}) ->
                             case gb_sets:is_member(Dep, AccS) of
                                 false ->
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, AccS),
                                      [Item | AccD]
                                     };
                                 true ->
                                     {AccQ, AccS, AccD}
                             end;
                        ({Dep, A, V, [raw]}, {AccQ, AccS, AccD}) ->
                             Item = {Dep, A, V},
                             case gb_sets:is_member(Dep, AccS) of
                                 false ->
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, AccS),
                                      [Item | AccD]
                                     };
                                 true ->
                                     {AccQ, AccS, AccD}
                             end;
                        (Item, {AccQ, AccS, AccD}) ->
                             ?WARN("Drop ~p", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Module, Dir, NewQ, NewS, DownloadL, DownL,
                     NewAccResult, Delay, Args);
        {{value, _}, _} ->
            {ok, NewAccResult};
        {empty, _} ->
            {ok, NewAccResult}
    end.

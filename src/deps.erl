-module(deps).

-export([
         foreach/3,
         foreach/4,
         foreach/5
        ]).

-include("rebar.hrl").

-define(ROOT, bfs_root).
-callback do(Dir :: string(), App :: atom(), _VSN, _Source) ->
    {nothing, App :: atom()} |
    {ok, App :: atom(), Output :: string()} |
    {error, App :: atom(), Reason :: string()}.

-spec foreach(Dir :: string(), Module :: module(), _Acc) -> {ok, _Res} | {error, _Reason}.
foreach(Dir, Module, Acc) ->
    foreach(Dir, Module, Acc, false, ?REBAR_CFG).
foreach(Dir, Module, Acc, RebarCfg) ->
    foreach(Dir, Module, Acc, false, RebarCfg).

foreach(Dir, Module, Acc, Delay, RebarCfg) ->
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
    bfs_step(Module, DepsFolder, DepsList, Acc, Delay).

-spec bfs_step(Module :: module(), Dir :: string(),
               DownloadList :: list({_, _, _} | {_, _}),
               _AccResult, Delay :: boolean()) -> {ok, list()} | none.
bfs_step(Module, Dir, DepsList, AccResult, Delay) ->
    Q = queue:from_list(DepsList),
    case lists:member(?ROOT, erlang:registered()) of
        true ->
            none;
        false ->
            true = erlang:register(?ROOT, self())
    end,
    bfs_step(Module, Dir, Q, gb_sets:new(), DepsList, gb_sets:new(), AccResult, Delay).

bfs_step(Module, Dir, Queue, ViewedDeps, DownloadList, DownloadedList, AccResult, Delay) ->
    CorrectDownList = lists:reverse(lists:foldl(
      fun(A = {App, VSN, Source}, Acc) ->
              % spawn(
                % fun() ->
                        Delay andalso timer:sleep(300),
                        io:format("App ~p~n", [A]),
                        ?ROOT ! Module:do(Dir, App, VSN, Source),
                % end),
              [A | Acc];
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
    (Size1 - 5 > Size2) andalso Delay andalso ?CONSOLE("Prepare ~p deps", [Size1]),

    case queue:out(Queue) of
        {{value, {App, _Vsn, _Source}}, Q} ->
            AppDir = filename:join([Dir, App, ?REBAR_CFG]),
            Child = case file:consult(AppDir) of
                {ok,    Config} -> proplists:get_value(deps, Config, []);
                {error, enoent} -> []
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
                        (Item, {AccQ, AccS, AccD}) ->
                             ?WARN("Drop ~p", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Module, Dir, NewQ, NewS, DownloadL, DownL,
                     NewAccResult, Delay);
        {empty, _} ->
            {ok, NewAccResult}
    end.

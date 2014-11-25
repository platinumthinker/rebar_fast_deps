-module(deps).

-export([
         foreach/2
        ]).

-include("rebar.hrl").

-define(ROOT, bfs_root).
-callback do(Dir :: string(), App :: atom(), _VSN, _Source) ->
    {nothing, App :: atom()} |
    {ok, App :: atom(), Output :: string()} |
    {error, App :: atom(), Reason :: string()}.

-spec foreach(Dir :: string(), Module :: module()) -> ok | {error, _Reason}.
foreach(Dir, Module) ->
    ok = file:set_cwd(Dir),
    {DepsFldr, DepsList} = case file:consult(?REBAR_CFG) of
        {ok, Config} ->
            {
             proplists:get_value(deps_dir, Config, "deps"),
             proplists:get_value(deps, Config, [])
            };
        {error, enoent} -> {[], "deps"}
    end,
    Q = queue:from_list(DepsList),
    DepsFolder = filename:join(Dir, DepsFldr),
    erlang:register(?ROOT, self()),
    case bfs_step(Module, DepsFolder, Q, gb_sets:new(), DepsList, gb_sets:new()) of
        ok ->
            ok;
        none ->
            none
    end.

bfs_step(Module, Dir, Queue, ViewedDeps, DownloadList, DownloadedList) ->
    CorrectDownList = lists:reverse(lists:foldl(
      fun(A = {App, VSN, Source}, Acc) ->
              spawn(
                fun() ->
                        ?ROOT ! Module:do(Dir, App, VSN, Source)
                end),
              [A | Acc];
         (Drop, Acc) ->
              ?WARN("Drop ~p", [Drop]),
              Acc
      end, [], DownloadList)),

    DownL = lists:foldl(
      fun({_, _, _}, Acc) ->
              receive
                  {nothing, App} ->
                      gb_sets:add(App, Acc);
                  {ok, App, Output} ->
                      ?CONSOLE(Output, []),
                      gb_sets:add(App, Acc);
                  {error, App, Reason} ->
                      ?ERROR("\e[1m\e[31m~p\e[0m: ~n~p", [App, Reason]),
                      exit("Error when deps update")
              after ?TIMEOUT ->
                exit("Timeout when update dep")
              end;
         (_, Acc) -> Acc
      end, DownloadedList, CorrectDownList),

    Size1 = gb_sets:size(DownL),
    Size2 = gb_sets:size(DownloadedList),
    Size1 - 5 > Size2 andalso ?CONSOLE("Prepare ~p deps", [Size1]),

    case queue:out(Queue) of
        {{value, {App, _Vsn, _Source}}, Q} ->
            AppDir = filename:join(Dir, App),
            ok = file:set_cwd(AppDir),
            Child = case file:consult(?REBAR_CFG) of
                {ok,    Config} -> proplists:get_value(deps, Config, []);
                {error, enoent} -> []
            end,
            {NewQ, NewS, DownloadL} = lists:foldl(
                     fun(Item = {Dep, _, _}, {AccQ, AccS, AccD}) ->
                             case gb_sets:is_member(Dep, AccS) of
                                 false ->
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, ViewedDeps),
                                      [Item | AccD]
                                     };
                                 true ->
                                     {AccQ, AccS, AccD}
                             end;
                        (Item, {AccQ, AccS, AccD}) ->
                             ?WARN("Drop ~p", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Module, Dir, NewQ, NewS, DownloadL, DownL);
        {empty, _} ->
            none
    end.

-module(deps).

-export([
         foreach/2
        ]).

-include("rebar.hrl").

-define(ROOT, bfs_root).
-callback do(fun((Dir :: string(), App :: atom(), _VSN, _Source) ->
    nothing | {ok, _Output} | {error, _Reason})) -> ok.

-spec foreach(Dir :: string(), Module :: module()) -> ok | {error, _Reason}.
foreach(Dir, Module) ->
    ok = file:set_cwd(Dir),
    {DepsFldr, DepsList} = case file:consult(?REBAR_CFG) of
        {ok,    Config} ->
            {
             proplists:get_value(deps_dir, Config, []),
             proplists:get_value(deps, Config, "deps")
            };
        {error, enoent} -> {[], "deps"}
    end,
    Q = queue:from_list(DepsList),
    DepsFolder = filename:join(Dir, DepsFldr),
    erlang:register(?ROOT, self()),
    case bfs_step(Module, DepsFolder, Q, gb_sets:new(), DepsList) of
        ok ->
            ok;
        none ->
            none
    end.

bfs_step(Module, Dir, Queue, ViewedDeps, DownloadList) ->
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

    lists:foreach(
      fun({_, _, _}) ->
              receive
                  nothing ->
                      nothing;
                  {ok, Output} ->
                      ?CONSOLE(Output, []);
                  {error, Reason} ->
                      ?ERROR(Reason, [])
              after ?TIMEOUT ->
                exit("Timeout when update dep")
              end;
         (_) -> nothing
      end, CorrectDownList),

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
                             ?WARN("Drop ~p~n", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Module, Dir, NewQ, NewS, DownloadL);
        {empty, _} ->
            none
    end.

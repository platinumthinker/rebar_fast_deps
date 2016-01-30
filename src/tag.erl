-module(tag).
-behaviour(deps).

-export([
         print/1,
         create/2,
         create/3,
         do/5
        ]).

-include("rebar.hrl").

print(Dir) ->
    {ok, Res} = deps:foreach(Dir, ?MODULE, [], []),
    UniqRes = lists:foldl(fun({_, _, Val}, Acc) ->
        lists:umerge(Val, Acc)
    end, [], Res),
    lists:foreach(fun(Val) ->
        ?CONSOLE("~s", [Val])
    end, UniqRes).

create(Dir, Tag) -> create(Dir, Tag, []).
create(Dir, Tag, IgnoredApp) ->
    {ok, Res} = deps:foreach(Dir, ?MODULE, [], []),
    Cmd = "git tag ~s && git push origin ~s",
    lists:foreach(
      fun({App, AppDir, Val}) ->
        case lists:member(App, IgnoredApp) orelse lists:member(Tag, Val) of
            true ->
                ?CONSOLE("Ignoring \e[31m~p\e[0m: ~p", [App, Tag]);
            false ->
                {ok, _} = updater:cmd(AppDir, Cmd, [Tag, Tag])
        end
    end, Res),
    ok.

do(Dir, App, _VSN, _Source, []) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager tag",
    {ok, Res} = updater:cmd(AppDir, Cmd, []),
    {accum, App, {erlang:atom_to_list(App), AppDir, Res}}.

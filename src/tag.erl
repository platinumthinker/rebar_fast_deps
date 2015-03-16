-module(tag).
-behaviour(deps).

-export([
         print/1,
         create/3,
         do/4
        ]).

-include("rebar.hrl").

print(Dir) ->
    {ok, Res} = deps:foreach(Dir, ?MODULE, []),
    UniqRes = lists:foldl(fun({_, _, Val}, Acc) ->
        lists:umerge(Val, Acc)
    end, [], Res),
    lists:foreach(fun(Val) ->
        ?CONSOLE("~s", [Val])
    end, UniqRes).

create(Dir, Tag, IgnoredApp) ->
    {ok, Res} = deps:foreach(Dir, ?MODULE, []),
    Cmd = "git tag ~s && git push origin ~s",
    lists:foreach(
      fun({App, AppDir, _}) ->
        case lists:member(App, IgnoredApp) of
            true -> ok;
            false ->
                {ok, _} = updater:cmd(AppDir, Cmd, [Tag, Tag])
        end
    end, Res),
    ok.

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager tag --list '*.*.*'",
    {ok, Res} = updater:cmd(AppDir, Cmd, []),
    {accum, App, {erlang:atom_to_list(App), AppDir, Res}}.
-module(checker).

-export([
         checker/1,
         do/4
        ]).

-include("rebar.hrl").

-spec checker(Dir :: string()) -> ok | error.
checker(Dir) ->
    status:status(Dir),
    {ok, Deps} = deps:foreach(Dir, ?MODULE, []),
    {ok, Config} = file:consult(filename:join(Dir, ?REBAR_CFG)),
    DepsFldr = proplists:get_value(deps_dir, Config, "deps"),
    Dirs = filelib:wildcard(filename:join([Dir, DepsFldr, "*"])),
    lists:foreach(
      fun(Val) ->
              case filelib:is_dir(Val) andalso not lists:member({Val}, Deps) of
                  true ->
                      ?CONSOLE("\e[1m\e[33m~s\e[0m:~nUnused dir ~p",
                               [filename:basename(Val), Val]);
                  false ->
                      none
              end
      end, Dirs).

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    {accum, App, {AppDir}}.

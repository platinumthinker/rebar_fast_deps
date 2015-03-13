-module(status).

-export([
         status/1,
         do/4
        ]).

-include("rebar.hrl").

-spec status(Dir :: string()) -> ok | error.
status(Dir) ->
    deps:foreach(Dir, ?MODULE, ok).

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git status --short",
    case updater:cmd(AppDir, Cmd, []) of
        {ok, []} ->
            {accum, App, ok};
        {ok, Res} ->
            Format = case erlang:length(Res) of
                Len when Len > 1 ->
                    string:copies("~s~n", Len - 1) ++ "~s";
                Len ->
                    string:copies("~s", Len)
            end,
            ColorOut = string:concat("\e[1m\e[32m~p\e[0m:~n", Format),
            {ok, App, io_lib:format(ColorOut, [App | Res])};
        {error, Reason} ->
            ?CONSOLE("~p: ~p -> ~p ~n", [App, Dir, Reason])
    end.

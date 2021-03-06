-module(status).
-behaviour(deps).
-export([
         status/1,
         do/5,
         format/2
        ]).

-include("rebar.hrl").

-spec status(Dir :: string()) -> {ok | error, _Reason}.
status(Dir) ->
    deps:foreach(Dir, ?MODULE, ok, []).

do(Dir, App, _VSN, _Source, []) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git status --short",
    case updater:cmd(AppDir, Cmd, []) of
        {ok, []} ->
            {accum, App, ok};
        {ok, Res} ->
            Out = format(App, Res),
            {ok, App, Out};
        {error, {1, []}} ->
            {ok, App, io_lib:format("\e[31m~p\e[0m:~nDon't find in ~p",
                                    [App, AppDir])};
        {error, Reason} ->
            {ok, App, io_lib:format("\e[31mError in ~p\e[0m:~n~p",
                                    [App, Reason])}
    end.

format(App, Res) ->
    Format = case erlang:length(Res) of
                 Len when Len > 1 ->
                     string:copies("~s~n", Len - 1) ++ "~s";
                 Len ->
                     string:copies("~s", Len)
             end,
    ColorOut = string:concat("\e[32m~p\e[0m:~n", Format),
    io_lib:format(ColorOut, [App | Res]).

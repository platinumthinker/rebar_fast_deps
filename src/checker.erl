-module(checker).

-export([
         checker/1,
         do/4
        ]).

-include("rebar.hrl").

-spec checker(Dir :: string()) -> ok | error.
checker(Dir) ->
    deps:foreach(Dir, ?MODULE, ok).

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git status --short",
    {ok, Res} = updater:cmd(AppDir, Cmd, []),
    case Res of
        [] ->
            {accum, App, ok};
        _ ->
            % Format = case erlang:length(Res) of
            %     Len when Len > 1 ->
            %         string:copies("~s~n", Len - 1) ++ "~s";
            %     Len ->
            %         string:copies("~s", Len)
            % end,
            % Out = io_lib:format(Format, Res),
            {ok, App, io_lib:format("\e[1m\e[32m~p\e[0m:~n~s", [App, ""])}
    end.

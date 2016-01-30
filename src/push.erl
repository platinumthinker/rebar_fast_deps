-module(push).
-behaviour(deps).
-export([
         all/1,
         do/5
        ]).

-include("rebar.hrl").

-define(PUSH_FMT, "Push \e[32m~p\e[0m in origin/\e[32m~s\e[0m").
-define(DETACH_FMT, "Detached \e[31m~p\e[0m ~p").

-spec all(Dir :: string()) -> ok | error.
all(Dir) ->
    {ok, Changes} = deps:foreach(Dir, ?MODULE, [],[]),
    lists:foreach(
      fun({App, AppDir, change, Status = true}) ->
              Cmd1 = "git rev-parse --abbrev-ref HEAD",
              case updater:cmd(AppDir, Cmd1, []) of
                  {ok, ["HEAD"]} ->
                      ?CONSOLE(?DETACH_FMT, [App, Status]);
                  {ok, [Out]} ->
                      Cmd = "git push origin ~s",
                      case updater:cmd(AppDir, Cmd, Out) of
                          {ok, _} ->
                              ?CONSOLE(?PUSH_FMT, [App, Out]);
                          A ->
                              ?CONSOLE("\e[31mError:\e[0m ~p", [A])
                      end
              end;
         (_) -> none
      end, Changes).

do(Dir, App, _VSN, _Source,[]) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git status --short",
    Cmd1 = "git status",
    {ok, Res1} = updater:cmd(AppDir, Cmd1, []),
    Res2 = (nomatch /= re:run(lists:flatten(Res1), ".*by \\d+ commit.*")),
    case updater:cmd(AppDir, Cmd, []) of
        {ok, []} ->
            {accum, App, {App, none}};
        {ok, _Res} ->
            {accum, App, {App, AppDir, change, Res2}};
        {error, {1, []}} ->
            {ok, App, io_lib:format("\e[1m\e[31m~p\e[0m:~nDon't find in ~p",
                                    [App, AppDir])};
        {error, Reason} ->
            {ok, App, io_lib:format("\e[1m\e[31mError in ~p\e[0m:~n~p",
                                    [App, Reason])}
    end.

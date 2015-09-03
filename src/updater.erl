-module(updater).
-behaviour(deps).

-export([
         update_all/2,
         do/4,
         do/5,
         cmd/3
        ]).

-include("rebar.hrl").

-spec update_all(Dir :: string(), RebarCfg :: string()) -> ok | error.
update_all(Dir, RebarCfg) ->
    deps:foreach(Dir, ?MODULE, ok, RebarCfg).

do(Dir, App, _VSN, Source) ->
    do(Dir, App, _VSN, Source, false).
do(Dir, App, _VSN, Source, IsVerbose) ->
    AppDir = filename:join(Dir, App),
    try
        case filelib:is_dir(AppDir) of
            true ->
                case update_source(AppDir, Source, IsVerbose) of
                    [] -> {nothing, App};
                    _Line ->
                        {ok, App, io_lib:format("Update \e[1m\e[32m~p\e[0m from ~200p",
                                           [App, Source])}
                end;
            false ->
                download_source(AppDir, Source, IsVerbose),
                {ok, App, io_lib:format("Download \e[1m\e[32m~p\e[0m from ~200p",
                                        [App, Source])}
        end
    catch
        _:{badmatch, {error, {_Code, Reason}}} ->
            {error, App, Reason}
    end.

update_source(AppDir, {git, Url}, IsVerbose) ->
    update_source(AppDir, {git, Url, {branch, "master"}}, IsVerbose);
update_source(AppDir, {git, Url, ""}, IsVerbose) ->
    update_source(AppDir, {git, Url, {branch, "master"}}, IsVerbose);
update_source(AppDir, {git, _Url, {branch, Branch}}=Src, IsVerbose) ->
    Now = erlang:now(),
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q ~s", [Branch]),
    Now2 = erlang:now(),
    cmd(AppDir, "git pull --ff-only --no-rebase -q origin ~s", [Branch]),
    Now3 = erlang:now(),
    IsVerbose andalso
        io:format("update: ~10000p, fetch: ~10000pms, pull: ~10000pms~n",
            [Src, time_difference_ms(Now1, Now), time_difference_ms(Now3, Now2)]),
    Line;
update_source(AppDir, {git, _Url, {tag, Tag}}=Src, IsVerbose) ->
    Now = erlang:now(),
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q ~s", [Tag]),
    IsVerbose andalso
        io:format("update: ~1000p, fetch: ~1000pms~n", [Src, time_difference_ms(Now1, Now)]),
    Line;
update_source(AppDir, {git, _Url, Refspec}=Src, IsVerbose) ->
    Now = erlang:now(),
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q ~s", [Refspec]),
    IsVerbose andalso
        io:format("update: ~1000p, fetch: ~1000pms~n", [Src, time_difference_ms(Now1, Now)]),
    Line.

download_source(AppDir, {git, Url}, IsVerbose) ->
    download_source(AppDir, {git, Url, {branch, "master"}}, IsVerbose);
download_source(AppDir, {git, Url, ""}, IsVerbose) ->
    download_source(AppDir, {git, Url, {branch, "master"}}, IsVerbose);
download_source(AppDir, {git, Url, {branch, Branch}}=Src, IsVerbose) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    Now = erlang:now(),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q origin/~s", [Branch]),
    IsVerbose andalso
        io:format("download: ~1000p, clone: ~1000pms~n", [Src, time_difference_ms(Now1, Now)]);
download_source(AppDir, {git, Url, {tag, Tag}}=Src, IsVerbose) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    Now = erlang:now(),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q ~s", [Tag]),
    IsVerbose andalso
        io:format("download: ~1000p, clone: ~1000pms~n", [Src, time_difference_ms(Now1, Now)]);
download_source(AppDir, {git, Url, Rev}=Src, IsVerbose) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    Now = erlang:now(),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    Now1 = erlang:now(),
    cmd(AppDir, "git checkout -q ~s", [Rev]),
    IsVerbose andalso
        io:format("download: ~1000p, clone: ~1000pms~n", [Src, time_difference_ms(Now1, Now)]).

cmd(Dir, Str) ->
    cmd(Dir, Str, []).
cmd(Dir, Str, Args) ->
    try
        cmd(Dir, Str, Args, 0)
    catch
        Err ->
            io:format("Error ~p~n"
                      "Stacktrace: ~stack", [Err, erlang:get_stacktrace()]),
            erlang:halt(Err)
    end.

cmd(Dir, Str, Args, Retry) ->
    Port = erlang:open_port({spawn, ?FMT(Str, Args)}, [{cd, Dir}, exit_status,
                {line, 6558}, hide, stderr_to_stdout, binary]),
    case cmd_loop(Port, <<>>) of
        {ok, Output} -> {ok, Output};
        {error, Reason} ->
            if Retry + 1 > ?RETRY ->
                   {error, Reason};
               true ->
                   cmd(Dir, Str, Args, Retry + 1)
            end
    end.

cmd_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            cmd_loop(Port, <<Acc/binary, Line/binary, <<"\n">>/binary>>);
        {Port, {data, {noeol, Line}}} ->
            cmd_loop(Port, <<Acc/binary, Line/binary>>);
        {Port, {data, Line}} ->
            cmd_loop(Port, <<Acc/binary, Line/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, replace_eol(io_lib:format("~ts", [Acc]))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, replace_eol(io_lib:format("~ts", [Acc]))}}
    end.

replace_eol(Line) ->
    ReFlags = [global, unicode],
    [ binary_to_list(L) || L <- re:replace(Line, "\\R", "", ReFlags), L =/= [] ].


time_difference_ms({_, _, _} = FinishNow, {_, _, _} = StartNow) ->
    (now_to_ticks(FinishNow) - now_to_ticks(StartNow)) div 1000;

time_difference_ms({_, _, _} = _FinishNow, StartNow) ->
    erlang:error(badarg, [{now2, StartNow}]);

time_difference_ms(FinishNow, {_, _, _} = _StartNow) ->
    erlang:error(badarg, [{now1, FinishNow}]).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec now_to_ticks({MegaSeconds :: integer(), Seconds :: integer(), Miliseconds :: integer()}) -> integer().
%%--------------------------------------------------------------------
%% Преобразует время из формата now в числовое представление времени.
%%--------------------------------------------------------------------
now_to_ticks({MegaSeconds, Seconds, Miliseconds}) ->
    MegaSeconds * 1000000000000 + Seconds * 1000000 + Miliseconds.

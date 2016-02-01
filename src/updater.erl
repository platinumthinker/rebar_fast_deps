-module(updater).
-behaviour(deps).

-export([
         update_all/2,
         do/5,
         do/6,
         cmd/3
        ]).

-include("rebar.hrl").
-define(FMT_UPDATE, "Update \e[1m\e[32m~p\e[0m from ~200p").
-define(FMT_DOWNLOAD, "Download \e[1m\e[32m~p\e[0m from ~200p").

-spec update_all(Dir :: string(), RebarCfg :: string()) -> {ok | error, _Res}.
update_all(Dir, RebarCfg) ->
    deps:foreach(Dir, ?MODULE, ok, [], RebarCfg).

do(Dir, App, VSN, Source, []) ->
    do(Dir, App, VSN, Source, [], false).
do(Dir, App, _VSN, Source, [], _IsVerbose) ->
    AppDir = filename:join(Dir, App),
    try
        case filelib:is_dir(AppDir) of
            true ->
                case update_source(AppDir, Source) of
                    [] -> {nothing, App};
                    _Line ->
                        {ok, App, io_lib:format(?FMT_UPDATE, [App, Source])}
                end;
            false ->
                download_source(AppDir, Source),
                {ok, App, io_lib:format(?FMT_DOWNLOAD, [App, Source])}
        end
    catch
        _:{badmatch, {error, {_Code, Reason}}} ->
            {error, App, Reason}
    end.

update_source(AppDir, {git, Url, [raw]}) ->
    update_source(AppDir, {git, Url, {branch, "master"}});
update_source(AppDir, {git, Url, {branch, Branch}, [raw]}) ->
    update_source(AppDir, {git, Url, {branch, Branch}});
update_source(AppDir, {git, Url}) ->
    update_source(AppDir, {git, Url, {branch, "master"}});
update_source(AppDir, {git, Url, ""}) ->
    update_source(AppDir, {git, Url, {branch, "master"}});
update_source(AppDir, {git, _Url, {branch, Branch}}) ->
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Branch]),
    cmd(AppDir, "git pull --ff-only --no-rebase -q origin ~s", [Branch]),
    Line;
update_source(AppDir, {git, _Url, {tag, Tag}}) ->
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Tag]),
    Line;
update_source(AppDir, {git, _Url, Refspec}) ->
    {ok, Line} = cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Refspec]),
    Line.

download_source(AppDir, {git, Url}) ->
    download_source(AppDir, {git, Url, {branch, "master"}});
download_source(AppDir, {git, Url, {branch, Branch}, [raw]}) ->
    download_source(AppDir, {git, Url, {branch, Branch}});
download_source(AppDir, {git, Url, [raw]}) ->
    download_source(AppDir, {git, Url, {branch, "master"}});
download_source(AppDir, {git, Url, ""}) ->
    download_source(AppDir, {git, Url, {branch, "master"}});
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q origin/~s", [Branch]);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q ~s", [Tag]);
download_source(AppDir, {git, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    {ok, _} = cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q ~s", [Rev]).

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
                {line, 100}, hide, stderr_to_stdout, binary]),
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
%Вырезвает все пустые строки.
replace_eol(Line) ->
    [ binary_to_list(L) || L <- re:split(Line, "\\n",[unicode, {return, binary}]), L =/= <<>>] .

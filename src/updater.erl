-module(updater).
-behaviour(deps).

-export([
         update_all/2,
         do/4,
         cmd/3
        ]).

-include("rebar.hrl").

-spec update_all(Dir :: string(), RebarCfg :: string()) -> ok | error.
update_all(Dir, RebarCfg) ->
    deps:foreach(Dir, ?MODULE, ok, RebarCfg).

do(Dir, App, _VSN, Source) ->
    AppDir = filename:join(Dir, App),
    try
        case filelib:is_dir(AppDir) of
            true ->
                case update_source(AppDir, Source) of
                    [] -> {nothing, App};
                    _Line ->
                        {ok, App, io_lib:format("Update \e[1m\e[32m~p\e[0m from ~200p",
                                           [App, Source])}
                end;
            false ->
                download_source(AppDir, Source),
                {ok, App, io_lib:format("Download \e[1m\e[32m~p\e[0m from ~200p",
                                        [App, Source])}
        end
    catch
        _:{badmatch, {error, {_Code, Reason}}} ->
            {error, App, Reason}
    end.

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
    cmd(Dir, Str, Args, 0).
cmd(Dir, Str, Args, Retry) ->
    Port = erlang:open_port({spawn, ?FMT(Str, Args)}, [{cd, Dir}, exit_status,
                {line, 6558}, hide, stderr_to_stdout]),
    case cmd_loop(Port, []) of
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
            cmd_loop(Port, [Line ++ "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            cmd_loop(Port, [Line | Acc]);
        {Port, {data, Line}} ->
            cmd_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, replace_eol(lists:flatten(lists:reverse(Acc)))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, replace_eol(lists:flatten(lists:reverse(Acc)))}}
    end.

replace_eol(Line) ->
    ReFlags = [global, bsr_unicode],
    [ binary_to_list(L) || L <- re:replace(Line, "\\R", "", ReFlags), L =/= [] ].

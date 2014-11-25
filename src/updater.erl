-module(updater).
-behaviour(deps).

-export([
         update_all/1,
         do/4
        ]).

-include("rebar.hrl").

-spec update_all(Dir :: string()) -> ok | error.
update_all(Dir) ->
    deps:foreach(Dir, ?MODULE).

do(Dir, App, _VSN, Source) ->
    AppDir = filename:join(Dir, App),
    case filelib:is_dir(AppDir) of
        true ->
            A = update_source(AppDir, Source),
            erlang:display(A),
            {ok, io_lib:format("Update \e[1m\e[32m~p\e[0m from ~200p", [App, Source])};
        false ->
            A = download_source(AppDir, Source),
            erlang:display(A),
            {ok, io_lib:format("Download \e[1m\e[32m~p\e[0m from ~200p", [App, Source])}
    end.

update_source(AppDir, {git, Url}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, Url, ""}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, _Url, {branch, Branch}}) ->
    cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Branch]),
    cmd(AppDir, "git pull --ff-only --no-rebase -q origin ~s", [Branch]);
update_source(AppDir, {git, _Url, {tag, Tag}}) ->
    cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Tag]);
update_source(AppDir, {git, _Url, Refspec}) ->
    cmd(AppDir, "git fetch origin"),
    cmd(AppDir, "git checkout -q ~s", [Refspec]).

download_source(AppDir, {git, Url}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, ""}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q origin/~s", [Branch]);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q ~s", [Tag]);
download_source(AppDir, {git, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    Dir = filename:dirname(AppDir),
    Folder = filename:basename(AppDir),
    cmd(Dir, "git clone -n ~s ~s", [Url, Folder]),
    cmd(AppDir, "git checkout -q ~s", [Rev]).

cmd(Dir, Str) ->
    cmd(Dir, Str, []).
cmd(Dir, Str, Args) ->
    Port = erlang:open_port({spawn, ?FMT(Str, Args)}, [{cd, Dir}, exit_status,
                hide, stderr_to_stdout]),
    erlang:display({stat, cmd_loop(Port, [])}).
cmd_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            cmd_loop(Port, [Line ++ "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            cmd_loop(Port, [Line | Acc]);
        {Port, {data, Line}} ->
            cmd_loop(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}};
        {Port, Other} ->
            io:format("~p: other ~p~n", [Port, Other]),
            cmd_loop(Port, Acc)
    end.

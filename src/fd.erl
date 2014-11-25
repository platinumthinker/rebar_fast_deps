-module(fd).

-behaviour(deps).

-export([
         main/1,
         update_app/3,
         do/1
        ]).

-include("rebar.hrl").

main([Command]) ->
    {ok, Dir} = file:get_cwd(),
    main([Command, Dir]);
main([A, WD]) when A == "update"; A == "up" ->
    updater:bfs(WD);
main(["help", _]) ->
    io:format("Usage: fd <command> [path] (fast deps)~n"
              "Commands:"
              "  update (up) - For update rebar deps~n");
main(Args) ->
    io:format("Command ~p not recognized.~n", [Args]),
    main(["help"]).

update_app(Dir, App, Source) ->
    AppDir = filename:join(Dir, App),
    Res = case filelib:is_dir(AppDir) of
        true ->
            update_source(AppDir, Source),
            io_lib:format("Update ~p from ~p~n", [App, Source]);
        false ->
            download_source(AppDir, Source),
            io_lib:format("Download ~p from ~p~n", [App, Source])
    end,
    ?ROOT ! Res.

update_source(AppDir, {git, Url}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, Url, ""}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, _Url, {branch, Branch}}) ->
    ShOpts = [{cd, AppDir}, {use_stdout, false}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Branch]), ShOpts),
    rebar_utils:sh(?FMT("git pull --ff-only --no-rebase -q origin ~s", [Branch]),ShOpts);
update_source(AppDir, {git, _Url, {tag, Tag}}) ->
    ShOpts = [{cd, AppDir}, {use_stdout, false}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), ShOpts);
update_source(AppDir, {git, _Url, Refspec}) ->
    ShOpts = [{cd, AppDir}, {use_stdout, false}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Refspec]), ShOpts).

download_source(AppDir, {git, Url}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, ""}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    ShOpts = [{cd, filename:dirname(AppDir)}, {use_stdout, false}],
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   ShOpts),
    rebar_utils:sh(?FMT("git checkout -q origin/~s", [Branch]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    ShOpts = [{cd, filename:dirname(AppDir)}, {use_stdout, false}],
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    ShOpts = [{cd, filename:dirname(AppDir)}, {use_stdout, false}],
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Rev]), [{cd, AppDir}]).

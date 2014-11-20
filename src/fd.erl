-module(fd).

-export([
         main/1
        ]).

-include("rebar.hrl").
-define(REBAR_CFG, "rebar.config").
-define(ROOT, bfs_root).
-define(TIMEOUT, 20000).

main([Command]) ->
    {ok, Dir} = file:get_cwd(),
    main([Command, Dir]);
main([A, WD]) when A == "update"; A == "up" ->
    bfs(WD);
main(["help", _]) ->
    io:format("Usage: fd <command> [path] (fast deps)~n"
              "Commands:"
              "  update (up) - For update rebar deps~n");
main(Args) ->
    io:format("Command ~p not recognized.~n", [Args]),
    main(["help"]).

%% @doc Breadth-first search
bfs(Path) ->
    ok = file:set_cwd(Path),
    {DepsFldr, DepsList} = case file:consult(?REBAR_CFG) of
        {ok,    Config} ->
            {
             proplists:get_value(deps_dir, Config, []),
             proplists:get_value(deps, Config, "deps")
            };
        {error, enoent} -> {[], "deps"}
    end,
    Q = queue:from_list(DepsList),
    DepsFolder = filename:join(Path, DepsFldr),
    erlang:register(?ROOT, self()),
    case bfs_step(DepsFolder, Q, gb_sets:new(), DepsList) of
        ok ->
            ok;
        none ->
            none
    end.

bfs_step(Dir, Queue, ViewedDeps, DownloadList) ->
    lists:foreach(
      fun({App, _VSN, Source}) ->
              spawn(?MODULE, fun update_app/3, [Dir, App, Source])
      end, DownloadList),
    lists:foreach(
      fun({_}) ->
              receive
                  {_, Res} ->
                      io:format(Res)
              after ?TIMEOUT ->
                exit("Timeout when update dep")
              end
      end, DownloadList),

    case queue:out(Queue) of
        {{value, {App, _Vsn, _Source}}, Q} ->
            AppDir = filename:join(Dir, App),
            ok = file:set_cwd(AppDir),
            Child = case file:consult(?REBAR_CFG) of
                {ok,    Config} -> proplists:get_value(deps, Config, []);
                {error, enoent} -> []
            end,
            {NewQ, NewS, DownloadL} = lists:foldl(
                     fun(Item = {Dep, _, _}, {AccQ, AccS, AccD}) ->
                             case gb_sets:is_member(Dep, AccS) of
                                 false ->
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, ViewedDeps),
                                      [Dep | AccD]
                                     };
                                 true ->
                                     {AccQ, AccS, AccD}
                             end;
                        (Item, {AccQ, AccS, AccD}) ->
                             ?WARN("Drop ~p~n", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Dir, NewQ, NewS, DownloadL);
        {empty, _} ->
            none
    end.

update_app(Dir, App, Source) ->
    AppDir = filename:join(Dir, App),
    Res = case filelib:is_dir(AppDir) of
        true ->
            io_lib:format("Update ~p from ~p~n", [App, Source]),
            update_source(AppDir, Source);
        false ->
            io_lib:format("Download ~p from ~p~n", [App, Source]),
            download_source(AppDir, Source)
    end,
    ?ROOT ! Res.

update_source(AppDir, {git, Url}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, Url, ""}) ->
    update_source(AppDir, {git, Url, {branch, "HEAD"}});
update_source(AppDir, {git, _Url, {branch, Branch}}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Branch]), ShOpts),
    rebar_utils:sh(?FMT("git pull --ff-only --no-rebase -q origin ~s", [Branch]),ShOpts);
update_source(AppDir, {git, _Url, {tag, Tag}}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), ShOpts);
update_source(AppDir, {git, _Url, Refspec}) ->
    ShOpts = [{cd, AppDir}],
    rebar_utils:sh("git fetch origin", ShOpts),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Refspec]), ShOpts).

download_source(AppDir, {git, Url}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, ""}) ->
    download_source(AppDir, {git, Url, {branch, "HEAD"}});
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q origin/~s", [Branch]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), [{cd, AppDir}]);
download_source(AppDir, {git, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]),
                   [{cd, filename:dirname(AppDir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Rev]), [{cd, AppDir}]).

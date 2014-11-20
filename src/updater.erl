-module(updater).
-export([
         bfs/1,
         update_app/3
        ]).

-include("rebar.hrl").
-define(REBAR_CFG, "rebar.config").
-define(ROOT, bfs_root).
-define(TIMEOUT, 20000).

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
    case bfs_step(DepsFolder, Q, gb_sets:new(), DepsList, gb_sets:new()) of
        ok ->
            ok;
        none ->
            none
    end.

bfs_step(Dir, Queue, ViewedDeps, DownloadList, Downloaded) ->
    NewDwled = lists:foldl(
      fun({App, _VSN, Source}, AccD) ->
              spawn(?MODULE, update_app, [Dir, App, Source]),
              timer:sleep(30),
              gb_sets:add(App, AccD);
         (Drop, AccD) -> ?ERROR("Drop ~p", [Drop]), AccD
      end, Downloaded, DownloadList),
    lists:foreach(
      fun({_, _, _}) ->
              receive
                  Res ->
                      ?CONSOLE(Res, [])
              end;
         (_) -> nothing
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
                                     NewD = case gb_sets:is_member(Dep, NewDwled) of
                                         false -> [Item | AccD];
                                         true -> AccD
                                     end,
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, ViewedDeps),
                                      NewD
                                     };
                                 true ->
                                     {AccQ, AccS, AccD}
                             end;
                        (Item, {AccQ, AccS, AccD}) ->
                             ?WARN("Drop ~p~n", [Item]),
                             {AccQ, AccS, AccD}
                     end, {Q, ViewedDeps, []}, Child),
            bfs_step(Dir, NewQ, NewS, DownloadL, NewDwled);
        {empty, _} ->
            none
    end.

update_app(Dir, App, Source) ->
    AppDir = filename:join(Dir, App),
    Res = case filelib:is_dir(AppDir) of
        true ->
            update_source(AppDir, Source),
            io_lib:format("Update \e[1m\e[32m~p\e[0m from ~200p", [App, Source]);
        false ->
            download_source(AppDir, Source),
            io_lib:format("Download \e[1m\e[32m~p\e[0m from ~200p", [App, Source])
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

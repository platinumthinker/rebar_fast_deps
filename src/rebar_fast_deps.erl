-module(rebar_fast_deps).

-export([
         'fast-update-deps'/2
        ]).

-include_lib("rebar.hrl").

'fast-update-deps'(Config, _AppFile) ->
    bfs(Config),
    ok.

%% @doc Breadth-first search
bfs(Config) ->
    DepsList = rebar_config:get(Config, deps, []),
    Q = queue:from_list(DepsList),
    DepsFolder = case rebar_config:get(Config, deps_dir, []) of
        []  ->  undefined;
        Dir ->  filename:join(rebar_utils:get_cwd(), Dir)
    end,
    case bfs_step(DepsFolder, Q, gb_sets:new()) of
        ok ->
            ok;
        none ->
            none
    end.

bfs_step(Dir, Queue, ViewedDeps) ->
    case queue:out(Queue) of
        {{value, {App, _Vsn, Source}}, Q} ->
            ?CONSOLE("~p~n", [App]),
            AppDir = update_app(Dir, App, Source),
            ok = file:set_cwd(AppDir),
            Child = case file:consult("rebar.config") of
                {ok,    Config} -> proplists:get_value(deps, Config, []);
                {error, enoent} -> []
            end,
            ?DEBUG("Childs ~p~n", [[A || {A, _, _} <- Child]]),
            {NewQ, NewS} = lists:foldl(
                     fun(Item = {Dep, _, _}, {AccQ, AccS}) ->
                             case gb_sets:is_member(Dep, AccS) of
                                 false ->
                                     {
                                      queue:in(Item, AccQ),
                                      gb_sets:add(Dep, ViewedDeps)
                                     };
                                 true ->
                                     {AccQ, AccS}
                             end
                     end, {Q, ViewedDeps}, Child),
            bfs_step(Dir, NewQ, NewS);
        {empty, _} ->
            none
    end.

update_app(Dir, App, Source) ->
    AppDir = filename:join(Dir, App),
    {ok, _} = case filelib:is_dir(AppDir) of
        true ->
            update_source(AppDir, Source);
        false ->
            download_source(AppDir, Source)
    end,
    AppDir.

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

-module(get).

-behaviour(deps).

-export([all/2, app/3, do/5]).

-include("rebar.hrl").

-spec all(Dir :: string(), fast | no_fast) -> {ok | error, _Res}.
all(Dir, Fast) ->
    case filelib:is_file(?REBAR_SAVE_CFG) of
        true ->
            SavedDeps = deps:deps_map(?REBAR_SAVE_CFG),
            deps:foreach(Dir, ?MODULE, [], {SavedDeps, Fast}, ?REBAR_CFG);
        false ->
            updater:update_all(Dir, ?REBAR_CFG, Fast)
    end.

-spec app(Dir :: string(), App :: string() | atom(), fast | no_fast) ->
             {ok | error, _Res}.
app(Dir, App, Fast) when is_list(App) ->
    AppAtom = list_to_atom(App),
    app(Dir, AppAtom, Fast);
app(_Dir, App, Fast) ->
    DepsDir = deps:deps_dir(?REBAR_CFG),
    SavedDeps = deps:deps_map(?REBAR_SAVE_CFG),
    Deps = deps:deps_map(?REBAR_CFG),

    case maps:find(App, Deps) of
        {ok, Source} ->
            VSN = "", % TODO
            case do(DepsDir, App, VSN, Source, {SavedDeps, Fast}) of
                {ok, App, Output} ->
                    ?CONSOLE(Output, []);
                {nothing, App} ->
                    ?CONSOLE("App already exist", []);
                {error, App, Reason} ->
                    ?ERROR("\e[1m\e[31m~p\e[0m: ~n~p", [App, Reason]),
                    exit("Error when getting dependency")
            end;
        error ->
            ?ERROR("Error! App ~p does not exist. Please, add it in rebar.config", [App]),
            exit("Error: no app")
    end.

do(Dir, App, _VSN, OriginSource, {SavedDeps, Fast}) ->
    AppDir = filename:join(Dir, App),
    Source =
        case maps:find(App, SavedDeps) of
            {ok, SavedSource} ->
                SavedSource;
            error ->
                OriginSource
        end,
    try
        case filelib:is_dir(AppDir) of
            true ->
                Cmd = "git rev-parse HEAD",
                {ok, RefBefore} = updater:cmd(AppDir, Cmd, []),
                case updater:update_source(App, AppDir, Source) of
                    [] ->
                        case updater:cmd(AppDir, Cmd, []) of
                            {ok, Res} when Res == RefBefore ->
                                {nothing, App};
                            _ ->
                                {ok, App, io_lib:format(?FMT_UPDATE, [App, Source])}
                        end;
                    _Line ->
                        {ok, App, io_lib:format(?FMT_UPDATE, [App, Source])}
                end;
            false ->
                updater:download_source(AppDir, Source, Fast),
                {ok, App, io_lib:format(?FMT_DOWNLOAD, [App, Source])}
        end
    catch
        _:{badmatch, {error, {_Code, Reason}}} ->
            {error, App, Reason}
    end.

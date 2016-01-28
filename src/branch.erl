-module(branch).
-behaviour(deps).

-export([
         print/1,
         create/4,
         do/5
        ]).

-include("rebar.hrl").

print(Dir) ->
    {ok, Res} = deps:foreach(Dir, ?MODULE, []),
    UniqRes = lists:foldl(fun({_, _, Val, _, _}, Acc) ->
        lists:umerge(Val, Acc)
    end, [], Res),
    lists:foreach(fun(Val) ->
        case re:run(Val, "(.*)detached(.*)") of
            nomatch -> ?CONSOLE("~s", [Val]);
            _       -> none
        end
    end, lists:usort(UniqRes)).

create(Dir, Branch, IgnoredApp, Branches1) ->
    {ok, Res1} = deps:foreach(Dir, ?MODULE, []),
    Res = lists:usort(Res1),
    {Hashs, Branches} = lists:foldl(fun({App, _, Branch1, Hash, _}, {Acc, Acc2}) ->
        {[{App, Hash} | Acc], [{App, Branch1} | Acc2]}
    end, {[], []}, Res),
    lists:foreach(
      fun({App, AppDir, _, _, false}) ->
              Br = proplists:get_value(App, Branches),
              case lists:member(App, IgnoredApp) orelse lists:member(Branch, Br) of
                  true ->
                      ?CONSOLE("Ignoring \e[31m~s\e[0m: already exist ~p",
                               [App, Branch]);
                  false -> cfg_modifier(AppDir, Branch, Hashs, Branches1)
              end;
         (_) ->
              none
    end, Res),
    cfg_modifier(Dir, Branch, Hashs, Branches1),
    ok.


cfg_modifier(AppDir, Branch, Hashs, Branches) ->
    Cmd = "git checkout -b ~s",
    updater:cmd(AppDir, Cmd, [Branch]),
    Name = filename:join(AppDir, ?REBAR_CFG),
    case file:consult(Name) of
        {ok, Conf} ->
            Deps = proplists:get_value(deps, Conf, []),
            {Res, _} = lists:foldl(
                    fun (Val, Acc) ->
                            AppStr = atom_to_list(element(1, Val)),
                            Hash = proplists:get_value(AppStr, Hashs),
                            deps_modifier(Val, Acc, Hash, Branches)
                    end, {[], Branch}, lists:sort(Deps)),
            change_deps(Res, Conf, Name, Branch, AppDir);
        {error, enoent} ->
            none
    end.

change_deps([], _Conf, _Name, _Branch, _AppDir) ->
    none;
change_deps(Deps, Conf, Name, Branch, AppDir) ->
  NewDeps = lists:reverse(Deps),
  NewConf = lists:keyreplace(deps, 1, Conf, {deps, NewDeps}),
  {ok, F} = file:open(Name, [write]),
  io:fwrite(F, "~s ~s ~s~n~n",
            ["%% THIS FILE IS GENERATED FOR", Branch, "%%"]),
  [ io:fwrite(F, "~p.~n", [Item]) || Item <- NewConf ],
  io:fwrite(F, "~s", ["\n"]),
  file:close(F),
  Cmd1 = "git commit -am 'Create release branches ~s'",
  updater:cmd(AppDir, Cmd1, [Branch]).

deps_modifier({App, VSN, {git, Url}}, Acc, Hash, Branches) ->
    deps_modifier({App, VSN, {git, Url, {branch, "HEAD"}}}, Acc, Hash, Branches);
deps_modifier({App, VSN, {git, Url, ""}}, Acc, Hash, Branches) ->
    deps_modifier({App, VSN, {git, Url, {branch, "HEAD"}}}, Acc, Hash, Branches);
deps_modifier({App, VSN, {git, Url, {branch, "master"}}}, Acc, Hash, Branches) ->
    deps_modifier({App, VSN, {git, Url, {branch, "HEAD"}}}, Acc, Hash, Branches);
deps_modifier({App, VSN, {git, Url, {branch, Branch1}}}, {Acc, Branch}, Hash, Branches) ->
    case re:run(Url, "(.*):external(.*)") of
        nomatch ->
            case lists:member(Branch1, Branches ++ ["HEAD"]) of
                true ->
                    {[ {App, VSN, {git, Url, {branch, Branch}}} | Acc ], Branch};
                false ->
                    {[ {App, VSN, {git, Url, Hash}} | Acc ], Branch}
            end;
        {match, _} ->
            {[ {App, VSN, {git, Url, Hash}} | Acc ], Branch}
    end;
deps_modifier(Dep, {Acc, Branch}, _Hash, _Branches) ->
    {[ Dep | Acc ], Branch}.

do(Dir, App, _VSN, _Source,[]) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager branch --all",
    Cmd2 = "git --no-pager log -1 --oneline --pretty=tformat:'%h'",
    Cmd3 = "git config --get remote.origin.url",
    {ok, Hash} = updater:cmd(AppDir, Cmd2, []),
    {ok, Url} = updater:cmd(AppDir, Cmd3, []),
    External = re:run(Url, "(.*):external(.*)") /= nomatch,
    {ok, Res1} = updater:cmd(AppDir, Cmd, []),
    Res = lists:foldl(
      fun(Val, Acc) ->
              [string:sub_string(Val, 3) | Acc]
      end, [], Res1),
    {accum, App, {erlang:atom_to_list(App), AppDir, Res, Hash, External}}.

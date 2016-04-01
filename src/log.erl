%% -*- coding: utf-8 -*-
-module(log).
-behaviour(deps).

-export([
        show/1,
        do/5
        ]).

-include("rebar.hrl").

-spec show(Dir :: string()) -> ok | error.
show(Dir) ->
    {ok, Deps} = deps:foreach(Dir, ?MODULE, [], []),
    {FilteredDeps, MaxNum, MaxNum2} = lists:foldl(
        fun({App, B, C, Author, Msg}, {Acc, MAcc, MAcc2}) ->
                AuthorU = unicode:characters_to_list(list_to_binary(Author)),
                MsgU = unicode:characters_to_list(list_to_binary(Msg)),
                AppStr = erlang:atom_to_list(App),
                {
                 [ {AppStr, B, C, AuthorU, MsgU} | Acc],
                 erlang:max(string:len(AuthorU), MAcc),
                 erlang:max(string:len(AppStr), MAcc2)
                };
           (_, Acc) -> Acc
        end, {[], 0, 0}, Deps),
    Comparator = fun({_, A, _, _, _}, {_, B, _, _, _}) -> A > B end,
    SortList = lists:sort(Comparator, FilteredDeps),

    Delim = "\e[33m●\e[0m",
    lists:foreach(fun({App, _, Hash, Author, Msg}) ->
        AutCentr = string:copies(" ", (MaxNum -  string:len(Author))) ++ Author,
        AppCentr = string:centre(App, MaxNum2),
        End = case length(Msg) > 58 of
            true -> "..";
            false -> ""
        end,
        Format = "~s \e[34m~ts \e[0m~ts\e[32m~s\e[0m~ts ~.58ts~s",
        Args = [ Hash, AutCentr, Delim, AppCentr, Delim,
                 [ X || X <- Msg, X /= '\n'], End ],
        ?CONSOLE(Format, Args)
    end, SortList).

do(Dir, App, _VSN, _Source, []) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager log --quiet --pretty=tformat:'%at|||%h|||%an|||%s'",
    Res = case updater:cmd(AppDir, Cmd, []) of
        {ok, Res2} -> Res2;
        Error -> ?ERROR("Error ~p ~p => ~p", [Dir, App, Error]),
                 ?FAIL
    end,

    case Res of
        [] ->
            {ok, Res1} = updater:cmd(AppDir, Cmd, []),
            case Res1 of
                [] ->
                    {accum, App, App};
                _ ->
                    RegRes = filter_msgs(App, Res),
                    {accum, App, RegRes}
            end;
        _ ->
            RegRes = filter_msgs(App, Res),
            {accum, App, RegRes}
    end.

% three_month() ->
%     {{Y, M, D}, _} = calendar:local_time(),
%     case M - 3 of
%         NewM when NewM > 0 ->
%             {Y, NewM, D};
%         _NewM ->
%             {Y - 1, 12, D}
%     end.

% last_month() ->
%     {{Y, M, D}, _} = calendar:local_time(),
%     case D - 16 of
%         NewD when NewD > 0 ->
%             {Y, M, NewD};
%         NewD ->
%             case M - 1 of
%                 NewM when NewM > 0 ->
%                     {Y, NewM, calendar:last_day_of_the_month(Y, NewM) + NewD};
%                 _NewM ->
%                     {Y - 1, 12,
%                      calendar:last_day_of_the_month(Y - 1, 12) + NewD}
%             end
%     end.

filter_msgs(App, Msgs) ->
  RegEx = "(.*)\\|\\|\\|(.*)\\|\\|\\|(.*)\\|\\|\\|(.*)",
  RegOpt = [{capture, [1, 2, 3, 4], list}, unicode],
  lists:foldl(
             fun(Line, Acc) ->
                     case re:run(Line, RegEx, RegOpt) of
                         {match, [Time, Hash, Author, Msg]} ->
                             [{App, Time, Hash, Author, Msg} | Acc];
                         _ ->
                             Acc
                     end
             end, [], Msgs).

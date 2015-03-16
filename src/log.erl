-module(log).
-behaviour(deps).

-export([
        show/1,
        do/4
        ]).

-include("rebar.hrl").

-spec show(Dir :: string()) -> ok | error.
show(Dir) ->
    {ok, Deps} = deps:foreach(Dir, ?MODULE, []),
    {FilteredDeps, MaxNum, MaxNum2} = lists:foldl(
        fun({App, B, C, Author, D}, {Acc, MAcc, MAcc2}) ->
                AppStr = erlang:atom_to_list(App),
                {
                 [ {AppStr, B, C, Author, D} | Acc],
                 erlang:max(erlang:length(Author), MAcc),
                 erlang:max(erlang:length(AppStr), MAcc2)
                };
           (_, Acc) -> Acc
        end, {[], 0, 0}, Deps),
    Comparator = fun({_, A, _, _, _}, {_, B, _, _, _}) -> A > B end,
    SortList = lists:sort(Comparator, FilteredDeps),

    Format = "~s \e[34m~s\e[0m ~s ~ts\e[32m~s\e[0m~s ~ts~.120ts~s",
    Delim = "\e[33m● \e[0m",
    lists:foreach(fun({App, _, Hash, Author, Msg}) ->
        Spaces = string:copies(" ", MaxNum - length(Author)),
        Spaces2 = string:copies(" ", MaxNum2 - length(App)),
        End = case length(Msg) > 120 of
            true -> "...";
            false -> ""
        end,
        ?CONSOLE(Format, [Hash, Author, Spaces, Delim, App, Spaces2, Delim, Msg,
                         End])
    end, SortList).

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager log --quiet --pretty=tformat:'%at|||%h|||%an|||%s' --after='~p'",
    {ok, Res} = updater:cmd(AppDir, Cmd, [last_month()]),
    case Res of
        [] ->
            {accum, App, App};
        _ ->
            RegEx = "(.*)\\|\\|\\|(.*)\\|\\|\\|(.*)\\|\\|\\|(.*)",
            RegOpt = [{capture, [1, 2, 3, 4], list}, unicode],
            RegRes = lists:foldl(
              fun(Line, Acc) ->
                      case re:run(Line, RegEx, RegOpt) of
                          {match, [Time, Hash, Author, Msg]} ->
                              [{App, Time, Hash, Author, Msg} | Acc];
                          _ ->
                              Acc
                      end
              end, [], Res),
            {accum, App, RegRes}
    end.

last_month() ->
    {{Y, M, D}, _} = calendar:local_time(),
    case D - 16 of
        NewD when NewD > 0 ->
            {Y, M, NewD};
        NewD ->
            case M - 1 of
                NewM when NewM > 0 ->
                    {Y, NewM, calendar:last_day_of_the_month(Y, NewM) + NewD};
                _NewM ->
                    {Y - 1, 12, calendar:last_day_of_the_month(Y - 1, 12) + NewD}
            end
    end.



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
    {FilteredDeps, MaxNum} = lists:foldl(
        fun({App, _, _, _, _} = Val, {Acc, MAcc}) ->
                {
                 [Val | Acc],
                 erlang:max(erlang:length(erlang:atom_to_list(App)), MAcc)
                };
           (_, Acc) -> Acc
        end, {[], 0}, Deps),
    Comparator = fun({_, A, _, _, _}, {_, B, _, _, _}) -> A > B end,
    SortList = lists:sort(Comparator, FilteredDeps),

    Format = "~p: ~s ~s ~s",
    lists:foreach(fun({App, _, Hash, Author, Msg}) ->
        ?CONSOLE(Format, [App, Hash, Author, Msg])
    end, SortList),
    ?CONSOLE("From date:~p", [last_month()]).

do(Dir, App, _VSN, _Source) ->
    AppDir = filename:join(Dir, App),
    Cmd = "git --no-pager log --quiet --pretty=format:'%at|||%h|||%an|||%s%n' --after='~p'",
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
    case D - 14 of
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



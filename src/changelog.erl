-module(changelog).
-behaviour(deps).

-export([
         create/1,
         do/5,
         write_commits/1
        ]).

-include("rebar.hrl").
-define(ADD, addDeps).

-spec create([string()]) -> ok | error.

create([Dir, Option]) ->
    try
        ets:new(?ADD, [duplicate_bag, public, named_table]),
        {Count, DirRebarSave} = case get_param(Option) of
            {ok, OptionList}->
                {
                    proplists:get_value('max-count', OptionList, ""),
                    proplists:get_value('dir', OptionList, "rebar.config.save")
                };
            {error, ReasonErr}-> exit(ReasonErr)
        end,
        case filelib:is_file(DirRebarSave) of
            true ->
                case file:consult(DirRebarSave) of
                    {ok, List} ->
                        ets:new(?MODULE, [duplicate_bag, public, named_table]),
                        SelfHash = proplists:get_value(self_hash, List, ""),
                        {Cmd, Args} = form_cmd(SelfHash, Count),
                        {ok, Res1} = updater:cmd(Dir, Cmd, Args),
                        try
                            {ok, [{application, ReleaseName, _}]} =
                            file:consult(filelib:wildcard("src/*.app.src")),
                            write_commits({ReleaseName, Res1})
                        catch
                            _:_ -> ok
                        end,
                        DepsOld = proplists:get_value(deps, List, []),
                        lists:foreach(
                          fun({_App , _VSN, {git, _, [raw]}})-> ok;
                             ({_App , _VSN, {git, _, _, [raw]}})-> ok;

                             ({App , _VSN, {git, _, [Hash]}})->
                                  ets:insert(?MODULE, {App, Hash});

                             ({App, _VSN, {git, _, {tag, Hash}}})->
                                  ets:insert(?MODULE, {App, Hash});

                             ({App, _VSN, {git, _, Hash}})->
                                  ets:insert(?MODULE, {App, Hash})
                          end, DepsOld);

                    {error, Reason} ->
                        exit("Error: " ++ Reason)
                end;
            false->
                exit("File " ++ DirRebarSave ++ " does not exists. "
		     "Run 'fd save' before.")
        end,
        {ok, ListDeps} = case Count of
            ""->
                deps:foreach(Dir, ?MODULE, [], Count);
            X->
                deps:foreach(Dir, ?MODULE, [], "--max-count=" ++ X)
        end,
        lists:foreach(fun write_commits/1, ListDeps),
        case ets:first(?ADD) of
            '$end_of_table'-> ok;
            FirstAdd->
                io:format("  * ~s~n", ["Dependences add to project"]),
                check_table_add(FirstAdd)
        end,

        case ets:first(?MODULE) of
            '$end_of_table'-> ok;

            FirstDel->
                io:format("  * ~s~n", ["Dependences deleted from project"]),
                check_table_delete(FirstDel)
        end
    catch
        _:E:S -> io:format("Error ~p~n~p~n", [E, S])
    end.


write_commits([]) -> ok;
write_commits([First|Other])->
    io:format("    - ~ts~n", [unicode:characters_to_list(list_to_binary(First))]),
    write_commits(Other);

write_commits({_, []}) -> ok;
write_commits({App, Commits})->
    io:format("  * ~p.~n", [App]),
    % Распечатывается список коммитов для зависимостей
    % которые ранее присутствовали в проекте и остались
    % на данный момент , но в них были изменения
    write_commits(Commits).


do(Dir, App, _VSN, _Source, Count) ->
    AppDir = filename:join(Dir, App),
    Res = case ets:lookup(?MODULE, App) of
        []->
            ets:delete(?MODULE, App),
            ets:insert(?ADD, {App, add}),
            [];
        [{App, raw}]->
            ets:delete(?MODULE, App),
            [];
        [{App, Hash}]->
            Cmd = "git --no-pager log ~s --pretty=format:\"%s%n\" --reverse ~s..",
            Res2 = case updater:cmd(AppDir, Cmd, [Count, Hash]) of
                        {ok, Res1} -> Res1;
                        _ -> ""
                    end,
            ets:delete(?MODULE, App),
            Res2
    end,
    {accum, App, {App, Res}}.


check_table_add('$end_of_table')->
    ets:delete(?ADD);
check_table_add(Key)->
    io:format("    - ~p.~n", [Key]),
    % Проверка таблицы на оставшиеся
    % зависимости , они являются убранными
    % с текущей сборки.
    check_table_add(ets:next(?ADD, Key)).


check_table_delete('$end_of_table')->
    ets:delete(?MODULE),
    ok;
check_table_delete(Key)->
    io:format("    - ~p.~n", [Key]),
    % Распечатка таблицы с зависимостями
    % которые добавились начиная с данной сборки.
    check_table_delete(ets:next(?MODULE, Key)).

get_param(Option)->
    get_param(Option, []).

get_param([], Res)->
    {ok, lists:reverse(Res)};
get_param([[$-, $-|A]|[]], _Res)->
    {error, "Option " ++ A ++ " is lost"};
get_param([[$-, $-|A], [$-, $-|_B]|_T], _Res)->
    {error, "Option " ++ A ++ " is lost"};
get_param([A, B|T], Res)->
    O = re:replace(list_to_binary(A), "--", "", [global, {return, list}]),
    get_param(T, [{list_to_atom(O), B}|Res]);
get_param(Other, _) ->
    {error, lists:flatten(io_lib:format("Unknown option: ~p", [Other]))}.


form_cmd("", "") ->
    {"git --no-pager log --pretty=format:\"%s%n\" --reverse", []};
form_cmd("", Count) ->
    {"git --no-pager log --max-count=~s --pretty=format:\"%s%n\" --reverse",
     [Count]};
form_cmd(SelfHash, "") ->
    {"git --no-pager log --pretty=format:\"%s%n\" --reverse ~s..", [SelfHash]};
form_cmd(SelfHash, Count) ->
    {"git --no-pager log --max-count=~s --pretty=format:\"%s%n\" --reverse ~s..",
     [Count, SelfHash]}.

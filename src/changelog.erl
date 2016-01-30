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
	ets:new(?ADD, [duplicate_bag, public, named_table]),

	{Count, DirRebarSave} = case get_param(Option) of
		{ok, OptionList}->
			{
				proplists:get_value('max-count', OptionList, ""),
				proplists:get_value('dir', OptionList, "rebar.config.save")
			};
		{error, ReasonErr}->
			io:format("~s~n", [ReasonErr]),
			{"", "rebar.config.save"}
	end,

	case
    filelib:is_file(DirRebarSave) of
    	true->
        case
        	file:consult(DirRebarSave) of
            	{ok, List} ->
					ets:new(?MODULE, [duplicate_bag, public, named_table]),
                    SelfHash = proplists:get_value(self_hash, List, []),
                    Cmd="git --no-pager log " ++ Count ++ " --pretty=format:\"%s%n\" --reverse "++ SelfHash ++ "..",
                    {ok, Res1} = updater:cmd(Dir, Cmd, []),
                    {ok, [{application, ReleaseName, _}]}=file:consult(filelib:wildcard("src/*.app.src")),
                    write_commits({ReleaseName, Res1}),
		    	  	DepsOld = proplists:get_value(deps, List, []),
                	lists:foreach(
				                fun({App , _VSN, {git, _, [Hash]}})->
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
            exit("File " ++ DirRebarSave ++ " does not exist")
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
    end.

write_commits([])->ok;
write_commits([First|Other])->
    io:format("    - ~s~n", [First]),
    write_commits(Other);

write_commits({_, []})->ok;
write_commits({App, Commits})->
    io:format("  * ~p.~n", [App]),
    write_commits(Commits).% Распечатывается список коммитов для зависимостей
% которые ранее присутствовали в проекте и остались
% на данный момент , но в них были изменения


do(Dir, App, _VSN, _Source, Count) ->
    AppDir = filename:join(Dir, App),
	Res = case ets:lookup(?MODULE, App) of
		[]->
			ets:delete(?MODULE, App),
			ets:insert(?ADD, {App, add}),
            [];
		[{App, Hash}]->
			Cmd="git --no-pager log " ++ Count ++ " --pretty=format:\"%s%n\" --reverse " ++ Hash ++ "..",
			%io:format("~s",[Cmd]),
    		{ok, Res1} = updater:cmd(AppDir, Cmd, []),
			ets:delete(?MODULE, App),
            Res1
    end,
    {accum, App, {App, Res}}.


check_table_add('$end_of_table')->
    ets:delete(?ADD);
check_table_add(Key)->
    io:format("    - ~p.~n", [Key]),
    check_table_add(ets:next(?ADD, Key)). % Проверка таблицы на оставшиеся
% зависимости , они являются убранными
% с текущей сборки.


check_table_delete('$end_of_table')->
    ets:delete(?MODULE),
    ok;
check_table_delete(Key)->
    io:format("    - ~p.~n", [Key]),
    check_table_delete(ets:next(?MODULE, Key)). % Распечатка таблицы с зависимостями
% которые добавились начиная с данной сборки.

get_param(Option)->
	get_param(Option, []).

get_param([], Res)->
    {ok, lists:reverse(Res)};
get_param([[$-, $-|A]|[]], _Res)->
    {error, "Option " ++ A ++ " is lost"};
get_param([[$-, $-|A], [$-, $-|_B]|_T], _Res)->
    {error, "Option " ++ A ++ " is lost"};
get_param([A, B|T], Res)->
    O=re:replace(list_to_binary(A), "--", "", [global, {return, list}]),
    get_param(T, [{list_to_atom(O), B}|Res]). %Функция разбора параметров.

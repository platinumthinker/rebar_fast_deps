%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str ++ "~n", Args)).

%% Green
-define(DEBUG(Str, Args), io:format("\e[32m" ++ Str ++ "\e[0m~n", Args)).
%% Blue
-define(INFO(Str, Args), io:format("\e[34m" ++ Str ++ "\e[0m~n", Args)).
%% Yellow
-define(WARN(Str, Args), io:format("\e[33m" ++ Str ++ "\e[0m~n", Args)).
%% Red
-define(ERROR(Str, Args), io:format("\e[31m" ++ Str ++ "\e[0m~n", Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(REBAR_CFG, "rebar.config").
-define(REBAR_SAVE_CFG, "rebar.config.save").
-define(TIMEOUT, 200000).
-define(RETRY, 10).
-define(FMT_UPDATE, "Update \e[1m\e[32m~p\e[0m from ~200p").
-define(FMT_DOWNLOAD, "Download \e[1m\e[32m~p\e[0m from ~200p").

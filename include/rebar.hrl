%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str ++ "~n", Args)).

-define(DEBUG(Str, Args), io:format(Str ++ "~n", Args)).
-define(INFO(Str, Args), io:format(Str ++ "~n", Args)).
-define(WARN(Str, Args), io:format(Str ++ "~n", Args)).
-define(ERROR(Str, Args), io:format(Str ++ "~n", Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(REBAR_CFG, "rebar.config").
-define(TIMEOUT, 20000).

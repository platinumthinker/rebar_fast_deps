-module(rebar_fast_deps_plugin).

-export([
         'fast-update-deps'/2
        ]).

-include_lib("rebar/include/rebar.hrl").

'fast-update-deps'(Config, _AppFile) ->
    DepsDir = rebar_config:get(Config, deps_dir, "deps"),
    ?INFO("DEPS ~p", [DepsDir]).

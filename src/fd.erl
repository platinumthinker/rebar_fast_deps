-module(fd).

-export([
         main/1
        ]).

-include("rebar.hrl").

main([Command]) ->
    {ok, Dir} = file:get_cwd(),
    main([Command, Dir]);
main([A, WD]) when A == "update"; A == "up" ->
    updater:update_all([WD]);
main(["help", _]) ->
    io:format("Usage: fd <command> [path] (fast deps)~n"
              "Commands:"
              "  update (up) - For update rebar deps~n");
main(Args) ->
    io:format("Command ~p not recognized.~n", [Args]),
    main(["help"]).

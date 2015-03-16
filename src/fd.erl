-module(fd).

-export([
         main/1
        ]).

-include("rebar.hrl").

main([Command]) ->
    {ok, Dir} = file:get_cwd(),
    main([Command, Dir]);
main([A, WD]) when A == "update"; A == "up" ->
    updater:update_all([WD], ?REBAR_CFG);
main([A, WD]) when A == "status"; A == "st" ->
    checker:checker([WD]);
main([A, WD]) when A == "log"; A == "lg" ->
    log:show([WD]);
main([A, WD]) when A == "load"; A == "ld" ->
    updater:update_all([WD], ?REBAR_SAVE_CFG);
main([A, WD]) when A == "save"; A == "sa" ->
    save:save_all([WD]);
main(["tag" | Args]) ->
    {ok, Dir} = file:get_cwd(),
    main([Dir, "tag" | Args]);
main([WD, A]) when A == "tag"; A == "tg" ->
    tag:print([WD]);
main([WD, A, Tag]) when A == "tag"; A == "tg" ->
    tag:create([WD], Tag, []);
main([WD, A, Tag, "--ignore" | IgnoredApp]) when A == "tag"; A == "tg" ->
    tag:create([WD], Tag, IgnoredApp);
main([A, WD, Tag]) when A == "tag"; A == "tg" ->
    tag:create([WD], Tag);
main(["help", _]) ->
    io:format("Usage: fd <command> [path] (fast deps)~n"
              "Commands:~n"
              "  update (up) - For update rebar deps~n"
              "  status (st) - Get status rebar deps~n"
              "  save   (sa) - For create rebar.config.save with deps on current state~n"
              "  load   (ld) - For load state from rebar.config.save~n"
              "  log    (lg) - Show deps log~n"
              "  tag    (tg) - list tags~n"
              "  tag 0.0.0.1 --ignore folsom lagger - create tag 0.0.0.1 without ignores app~n");
main(Args) ->
    io:format("Command ~p not recognized.~n", [Args]),
    main(["help"]).

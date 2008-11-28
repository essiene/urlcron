-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").


new_anonymous_schedule_test() ->
    urlcron_scheduler:start(),
    Starttime = "starttime",
    Url = "url",
    Expected = {ok, autoname, {Starttime, Url}},
    ?assertEqual(Expected, urlcron_scheduler:new(Starttime, Url)),
    urlcron_scheduler:stop().

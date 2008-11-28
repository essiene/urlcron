-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").


new_named_schedule_test() ->
    urlcron_scheduler:start(),
    Name = "schedule01",
    Starttime = urlcron_util:get_future_time(60000),
    Url = "url",
    {ok, {Name, Starttime, Url}} = urlcron_scheduler:new(Name, Starttime, Url).

new_anonymous_schedule_test() ->
    Starttime = urlcron_util:get_future_time(60000),
    Url = "url",
    {ok, {"schedule." ++ _Details, Starttime, Url}} = urlcron_scheduler:new(Starttime, Url),
    urlcron_scheduler:stop().

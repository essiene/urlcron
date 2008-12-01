-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").


new_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")).

new_named_schedule_test() ->
    Name = "schedule01",
    Starttime = urlcron_util:get_future_time(600000),
    Url = "url",
    {ok, {Name, Starttime, Url}} = urlcron_scheduler:new(Name, Starttime, Url).

new_anonymous_schedule_test() ->
    Starttime = urlcron_util:get_future_time(600000),
    Url = "url",
    {ok, {"schedule." ++ _Details, Starttime, Url}} = urlcron_scheduler:new(Starttime, Url).

get_status_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    Url = "http://boo.com",
    {ok, {"schedule02", StartTime, Url}} = urlcron_scheduler:new("schedule02", StartTime, Url),

    Result = urlcron_scheduler:get_status("schedule02"),
    Expected = {"schedule02", StartTime, Url, enabled},

    ?assertEqual(Expected, Result).

get_status_not_found_test() ->
    Result = urlcron_scheduler:get_status(schedule02),
    Expected = {error, not_found},

    ?assertEqual(Expected, Result).


destroy_test() ->
    urlcron_scheduler:stop(),
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

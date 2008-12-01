-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").
-include("urlcron.hrl").


new_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")).

new_named_schedule_test() ->
    Starttime = urlcron_util:get_future_time(60000),
    ?assertEqual({ok, "schedule01"}, urlcron_scheduler:create("schedule01", Starttime, "url")).

new_anonymous_schedule_test() ->
    Starttime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(Starttime, "url"),
    ?assertEqual(1, string:str(Name, "schedule.")).

get_test() ->
    StartTime = urlcron_util:get_future_time(6000),
    {ok, "schedule02"} = urlcron_scheduler:create("schedule02", StartTime, "http://boo.com"),

    {ok, #schedule{name=Name, start_time=StartTime1, url=Url, status=Status}} = urlcron_scheduler:get("schedule02"),
    Expected = {"schedule02", StartTime, "http://boo.com", enabled},
    ?assertEqual(Expected, {Name, StartTime1, Url, Status}).

get_status_not_found_test() ->
    Result = urlcron_scheduler:get("schedule03"),
    Expected = {error, not_found},
    ?assertEqual(Expected, Result).

destroy_test() ->
    urlcron_scheduler:stop(),
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

-module(test_schedule_store).
-include_lib("eunit/include/eunit.hrl").
-include("urlcron.hrl").

init_test() ->
    ?assertEqual(ok, schedule_store:start(erlcfg:new("urlcron.conf"))).

add_test() ->
    schedule_store:add("schedule1", self(), start_time, url, enabled),
    Result = schedule_store:get("schedule1"),
    Expected = #schedule{name="schedule1", pid=self(), start_time=start_time, url=url, status=enabled},
    ?assertEqual(Expected, Result).

get_non_existing_test() ->
    Result = schedule_store:get(sched135),
    Expected = {error, not_found},
    ?assertEqual(Expected, Result).

update_test() ->
    Schedule = schedule_store:get("schedule1"),
    Pid = Schedule#schedule.pid,
    TimeCreated = Schedule#schedule.time_created,
    UpdatedSchedule = Schedule#schedule{url="NewUrl", status=disabled, url_status=200},
    ?assertEqual(ok, schedule_store:update(UpdatedSchedule)),

    NewSchedule = schedule_store:get("schedule1"),
    Expected = #schedule{name="schedule1", pid=Pid, time_created=TimeCreated, status=disabled, url_status=200, start_time=start_time, url="NewUrl"},
    ?assertEqual(Expected, NewSchedule).

update_fields_test() ->
    Schedule = schedule_store:get("schedule1"),
    Pid = Schedule#schedule.pid,
    TimeCreated = Schedule#schedule.time_created,

    NewStartTime = urlcron_util:get_future_time(20000),
    ?assertEqual(ok, schedule_store:update("schedule1", [{url, "google.com"}, {start_time, NewStartTime}])),

    Expected = #schedule{name="schedule1", pid=Pid, time_created=TimeCreated, status=disabled, url_status=200, start_time=NewStartTime, url="google.com"},

    ?assertEqual(Expected, schedule_store:get("schedule1")).

update_fields_on_completed_test() ->
    Schedule = schedule_store:get("schedule1"),
    UpdatedSchedule = Schedule#schedule{status=completed},
    ?assertEqual(ok, schedule_store:update(UpdatedSchedule)),

    NewStartTime = urlcron_util:get_future_time(20000),
    ?assertEqual({error, schedule_already_completed}, schedule_store:update("schedule1", [{url, "google.com"}, {start_time, NewStartTime}])).
    
destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

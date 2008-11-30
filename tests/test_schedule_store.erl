-module(test_schedule_store).
-include_lib("eunit/include/eunit.hrl").
-include("urlcron.hrl").

init_test() ->
    ?assertEqual(ok, schedule_store:start(erlcfg:new("urlcron.conf"))).

add_test() ->
    schedule_store:add("schedule1", self(), start_time, url, enabled),
    Result = schedule_store:get("schedule1"),
    Expected = #schedule{name="schedule1", process=self(), start_time=start_time, url=url, status=enabled},
    ?assertEqual(Expected, Result).

get_non_existing_test() ->
    Result = schedule_store:get(sched135),
    Expected = {error, not_found},
    ?assertEqual(Expected, Result).

update_test() ->
    Schedule = schedule_store:get("schedule1"),
    Process = Schedule#schedule.process,
    TimeCreated = Schedule#schedule.time_created,
    UpdatedSchedule = Schedule#schedule{url="NewUrl", status=disabled, fetch_status=ok},
    schedule_store:update(UpdatedSchedule),

    NewSchedule = schedule_store:get("schedule1"),
    Expected = #schedule{name="schedule1", process=Process, time_created=TimeCreated, status=disabled, fetch_status=ok, start_time=start_time, url="NewUrl"},
    ?assertEqual(Expected, NewSchedule).

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).


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

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).


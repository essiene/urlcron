-module(test_schedule_store).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ?assertEqual(ok, schedule_store:start(erlcfg:new("urlcron.conf"))).

add_test() ->
    ?assertEqual(ok, schedule_store:add("schedule1", self(), start_time, url, enabled)).

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).


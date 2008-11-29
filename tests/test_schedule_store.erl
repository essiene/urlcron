-module(test_schedule_store).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ?assertEqual(ok, schedule_store:init(erlcfg:new())).

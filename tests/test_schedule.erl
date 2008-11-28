-module(test_schedule).
-include_lib("eunit/include/eunit.hrl").


start_enabled_test() ->
    StartTime = urlcron_util:time_from_now(60),
    {ok, Pid} = urlcron_schedule:start_link(StartTime, "url"),
    Timer = urlcron_schedule:get_timer(Pid),
    Status = urlcron_schedule:get_status(Pid),
    ?assertEqual({StartTime, "url", Timer, inactive_enabled}, Status),
    urlcron_schedule:stop(Pid).


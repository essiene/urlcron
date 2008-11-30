-module(test_schedule).
-include_lib("eunit/include/eunit.hrl").


start_enabled_test() ->
    StartTime = urlcron_util:get_future_time(600000),
    {ok, Pid} = urlcron_schedule:start_link("schedule01", StartTime),
    ?assert(is_process_alive(Pid) == true),
    urlcron_schedule:stop(Pid).

start_disabled_test() ->
    StartTime = urlcron_util:get_future_time(600000),
    {ok, Pid} = urlcron_schedule:start_link("schedule02", StartTime),
    ?assert(is_process_alive(Pid) == true),
    urlcron_schedule:stop(Pid).

schedule_runs_and_exists_test() ->
    inets:start(),
    urlcron_mochiweb:start(),
    StartTime = urlcron_util:get_future_time(1000),
    {ok, Pid} = urlcron_schedule:start_link("schedule03", StartTime),
    ?assert(is_process_alive(Pid) == true),
    timer:sleep(2000),
    ?assert(is_process_alive(Pid) == false),
    urlcron_mochiweb:stop(),
    inets:stop().


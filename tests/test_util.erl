-module(test_util).
-include_lib("eunit/include/eunit.hrl").


get_datetime_diff_test() ->
    A = erlang:localtime(),
    timer:sleep(2000),
    B = erlang:localtime(),
    Expected = 2000,
    ?assertEqual(Expected, urlcron_util:get_datetime_diff(B, A)).

now_diff_test() ->
    B = {{2008, 11, 28}, {15, 55, 17}},
    A = erlang:localtime(),
    Expected = urlcron_util:get_datetime_diff(B, A),
    ?assertEqual(Expected, urlcron_util:get_datetime_diff(B)).

gen_schedule_name_test() ->
    Name1 = "schedule." ++ _Details = urlcron_util:gen_schedule_name(),
    Name2 = urlcron_util:gen_schedule_name(),
    ?assertNot(Name1 == Name2).

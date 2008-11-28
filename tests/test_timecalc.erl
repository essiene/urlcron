-module(test_timecalc).
-include_lib("eunit/include/eunit.hrl").


date_diff_test() ->
    A = erlang:localtime(),
    timer:sleep(2000),
    B = erlang:localtime(),
    Expected = 2000,
    ?assertEqual(Expected, urlcron_timecalc:date_diff(B, A)).

now_diff_test() ->
    B = {{2008, 11, 28}, {15, 55, 17}},
    A = erlang:localtime(),
    Expected = urlcron_timecalc:date_diff(B, A),
    ?assertEqual(Expected, urlcron_timecalc:date_diff(B)).


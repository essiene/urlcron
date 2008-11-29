-module(test_webservice).
-include_lib("eunit/include/eunit.hrl").


create_new_schedule_test() ->
    urlcron_scheduler:start(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = urlcron_util:get_future_time(60000),

    QueryString = [
        {"name", "schedule1"}, 
        {"url", "url1"}, 
        {"year", integer_to_list(Year)}, 
        {"month",integer_to_list(Month)},
        {"day", integer_to_list(Day)},
        {"hour", integer_to_list(Hour)},
        {"minute", integer_to_list(Min)},
        {"seconds", integer_to_list(Sec)}
    ],

    Result = webservice:create_new_schedule(QueryString),
    StrDateTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Sec]),

    Expected = {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"schedule1">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"url1">>}
            ]
        },
    ?assertEqual(Expected, Result).


view_schedule_test() ->
    urlcron_scheduler:start(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = {{2008, 12, 11}, {16, 12, 12}},
    Result = webservice:view_schedule(),
    StrDateTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Sec]),

    Expected = [
        {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"http://localhost:8118">>}
            ]
        },
        {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"http://localhost:8118">>}
            ]
        },
        {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"http://localhost:8118">>}
            ]
        },
        {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"http://localhost:8118">>}
            ]
        }
    ],

    ?assertEqual(Expected, Result).
    
view_namedschedule_test() ->
    urlcron_scheduler:start(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = {{2008, 12, 11}, {16, 12, 12}},
    Result = webservice:view_schedule("xmas"),
    StrDateTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Sec]),

    Expected = {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"http://localhost:8118">>}
            ]
        },

    ?assertEqual(Expected, Result).    


delete_schedule_test() ->
    urlcron_scheduler:start(),
    Result = webservice:delete_schedule(),
    Expected = {struct,
            [
                {<<"status">>, 1},
                {<<"details">>, <<"All Schedules Deleted">>}
            ]
        },

    ?assertEqual(Expected, Result).

delete_namedschedule_test() ->
    urlcron_scheduler:start(),
    Result = webservice:delete_schedule("xmas"),
    Expected = {struct,
            [
                {<<"status">>, 1},
                {<<"name">>, <<"xmas">>},
                {<<"details">>, <<"Deleted">>}
            ]
        },

    ?assertEqual(Expected, Result).


update_schedule_test() ->
    urlcron_scheduler:start(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = urlcron_util:get_future_time(60000),

    QueryString = [
        {"url", "url1"}, 
        {"year", integer_to_list(Year)}, 
        {"month",integer_to_list(Month)},
        {"day", integer_to_list(Day)},
        {"hour", integer_to_list(Hour)},
        {"minute", integer_to_list(Min)},
        {"seconds", integer_to_list(Sec)}
    ],

    Result = webservice:update_schedule("schedule1", QueryString),
    StrDateTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Sec]),

    Expected = {struct, 
            [
                {<<"status">>, 1},
                {<<"name">>, <<"schedule1">>},
                {<<"starttime">>, list_to_binary(StrDateTime)},
                {<<"url">>, <<"url1">>}
            ]
        },
    ?assertEqual(Expected, Result).

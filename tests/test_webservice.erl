-module(test_webservice).
-include_lib("eunit/include/eunit.hrl").


create_new_schedule_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")),

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

    Expected = {struct, 
        [
            {<<"status">>, 1},
            {<<"name">>, <<"schedule1">>}
        ]
    },
    ?assertEqual(Expected, Result).

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

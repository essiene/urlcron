-module(test_webservice).
-include_lib("eunit/include/eunit.hrl").

-include("urlcron.hrl").

helper_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")).

create_new_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"name","xmas"},
        {"url","google"}
    ],
    Result = webservice:create_new_schedule(QueryString),
    ?assertEqual({ok, "xmas"}, Result).

enable_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"name","xmas"},
        {"url","google"}
    ],

    {ok, "xmas"} = webservice:create_new_schedule(QueryString),
    Result = webservice:enable_schedule("xmas"),
    ?assertEqual(ok, Result).

disable_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"name","xmas"},
        {"url","google"}
    ],

    {ok, "xmas"} = webservice:create_new_schedule(QueryString),
    Result = webservice:disable_schedule("xmas"),
    ?assertEqual(ok, Result).


view_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"name","xmas"},
        {"url","google"}
    ],
    {ok, "xmas"} = webservice:create_new_schedule(QueryString),

    {ok, Schedule} = webservice:view_schedule("xmas"),
    ?assertEqual(Schedule#schedule.name, "xmas").

delete_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"name","xmas"},
        {"url","google"}
    ],
    {ok, "xmas"} = webservice:create_new_schedule(QueryString),

    Result = webservice:delete_schedule("xmas"),
    ?assertEqual(ok, Result).

update_schedule_test() ->
    QueryString = [
        {"year","2008"},
        {"name","xmas"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"url","google"}
    ],
    {ok, "xmas"} = webservice:create_new_schedule(QueryString),

    QueryStringUpdate = [
        {"year","2008"},
        {"month","12"},
        {"day","13"},
        {"hour","15"},
        {"minute","22"},
        {"seconds","33"},
        {"url","google"}
    ],
    
    Result = webservice:update_schedule("xmas", QueryStringUpdate),
    ?assertEqual(ok, Result).

destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

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
        {"second","33"},
        {"name","xmase"},
        {"url","google"}
    ],
    Result = webservice:create_new_schedule(QueryString),
    ?assertEqual({ok, "xmase"}, Result).

update_schedule_test() ->
    QueryStringUpdate = [
        {"url","froogle"}
    ],

    Response = webservice:update_schedule("xmase", QueryStringUpdate),
    ?assertEqual(ok, Response).

disable_schedule_test() ->
    Result = webservice:disable_schedule("xmase"),
    ?assertEqual(ok, Result).

get_schedule_test() ->
    {ok, Schedule} = webservice:get_schedule("xmase"),
    ?assertEqual("xmase", Schedule#schedule.name),
    ?assertEqual(disabled, Schedule#schedule.status),
    ?assertEqual("froogle", Schedule#schedule.url).

enable_schedule_test() ->
    Result = webservice:enable_schedule("xmase"),
    ?assertEqual(ok, Result).


delete_schedule_test() ->
    Result = webservice:delete_schedule("xmase"),
    ?assertEqual(ok, Result),

    Result1 = webservice:get_schedule("xmase"),
    ?assertEqual({error, not_found}, Result1).


destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

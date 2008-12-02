-module(test_jsonutil).
-include_lib("eunit/include/eunit.hrl").
-include("urlcron.hrl").

helper_test() ->
    urlcron_scheduler:start(erlcfg:new("urlcron.conf")).
    

ok_test() ->
    Result = urlcron_jsonutil:to_json(ok),
    Expected = {struct,
        [
            {<<"status">>, 1},
            {<<"data">>, <<"Successful">>}
        ]
    },
    ?assertEqual(Expected, Result).

ok_name_test() ->
    Result = urlcron_jsonutil:to_json({ok, "Mine"}),
    Expected = {struct,
        [
            {<<"status">>, 1},
            {<<"data">>, <<"Mine">>}
        ]
    },
    ?assertEqual(Expected, Result).

error_reason_test() ->
    Result = urlcron_jsonutil:to_json({error, "Schedule Not Found"}),
    Expected = {struct,
        [
            {<<"status">>, 0},
            {<<"data">>, <<"Schedule Not Found">>}
        ]
    },
    ?assertEqual(Expected, Result).

time_test() ->
    Time = {{2008, 12, 12}, {15, 11, 32}},
    Result = urlcron_jsonutil:to_json(Time),
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = Time,
    Expected =  {struct,
       [
           {<<"year">>, Year},
           {<<"month">>, Month},
           {<<"day">>, Day},
           {<<"hour">>, Hour},
           {<<"minute">>, Minute},
           {<<"seconds">>, Seconds}                                                       
       ]
    },
    ?assertEqual(Expected, Result).

list_test() ->
    Result = urlcron_jsonutil:to_json("Name"),
    Expected = <<"Name">>,
    ?assertEqual(Expected, Result).

atom_test() ->
    Result = urlcron_jsonutil:to_json(schedulename),
    Expected = <<"schedulename">>,
    ?assertEqual(Expected, Result).

schedule_object_test() ->    
    Time = {{2008, 12, 12}, {15, 11, 32}},
    Url = "http://localhost:8118",
    Name = "schedule1",
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = Time,
    Schedule = #schedule{
                    name = Name,
                    start_time = Time,
                    status = enabled,
                    url = Url                        
                },
    Result = urlcron_jsonutil:to_json(Schedule),  
    
    {{YY, MM, DD}, {HH, MN, SS}} = urlcron_util:get_future_time(0),

    Expected = {struct,
        [
            {<<"name">>, <<"schedule1">>},
            {<<"pid">>, <<"undefined">>},
            {<<"start_time">>, {struct,
                                   [
                                       {<<"year">>, Year},
                                       {<<"month">>, Month},
                                       {<<"day">>, Day},
                                       {<<"hour">>, Hour},
                                       {<<"minute">>, Minute},
                                       {<<"seconds">>, Seconds}                                                       
                                   ]
                               }
            },
            {<<"time_created">>, {struct,
                                   [
                                       {<<"year">>, YY},
                                       {<<"month">>, MM},
                                       {<<"day">>, DD},
                                       {<<"hour">>, HH},
                                       {<<"minute">>, MN},
                                       {<<"seconds">>, SS}                                                       
                                   ]
                               }
            },
            {<<"time_started">>, <<"undefined">>},
            {<<"time_completed">>, <<"undefined">>},
            {<<"url">>, <<"http://localhost:8118">>},
            {<<"url_status">>, <<"undefined">>},
            {<<"url_headers">>, <<"undefined">>},
            {<<"url_content">>, <<"undefined">>},
            {<<"status">>, <<"enabled">>}
        ]
    },

    ?assertEqual(Expected, Result).

ok_schedule_object_test() ->
    Time = {{2008, 12, 12}, {15, 11, 32}},
    Url = "http://localhost:8118",
    Name = "schedule1",
    {{Year, Month, Day}, {Hour, Minute, Seconds}} = Time,
    Schedule = #schedule{
                    name = Name,
                    start_time = Time,
                    status = enabled,
                    url = Url                        
                },
    Result = urlcron_jsonutil:to_json({ok, Schedule}),  
    
    {{YY, MM, DD}, {HH, MN, SS}} = urlcron_util:get_future_time(0),

    Expected = {struct,        
        [
            {<<"status">>, 1},
            {<<"data">>, {struct,
                             [
                                {<<"name">>, <<"schedule1">>},
                                {<<"pid">>, <<"undefined">>},
                                {<<"start_time">>, {struct,
                                                       [
                                                           {<<"year">>, Year}, {<<"month">>, Month},{<<"day">>, Day},
                                                           {<<"hour">>, Hour}, {<<"minute">>, Minute},{<<"seconds">>, Seconds}                                                       
                                                       ]
                                                   }
                                },
                                {<<"time_created">>, {struct,
                                                       [
                                                           {<<"year">>, YY},{<<"month">>, MM},{<<"day">>, DD},
                                                           {<<"hour">>, HH},{<<"minute">>, MN},{<<"seconds">>, SS}                                                       
                                                       ]
                                                   }
                                },
                                {<<"time_started">>, <<"undefined">>},
                                {<<"time_completed">>, <<"undefined">>},
                                {<<"url">>, <<"http://localhost:8118">>},
                                {<<"url_status">>, <<"undefined">>},
                                {<<"url_headers">>, <<"undefined">>},
                                {<<"url_content">>, <<"undefined">>},
                                {<<"status">>, <<"enabled">>}
                             ]
                         }
            }
        ]
    },

    ?assertEqual(Expected, Result).


destroy_test() ->
    schedule_store:destroy(erlcfg:new("urlcron.conf")).

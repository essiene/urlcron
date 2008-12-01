-module(urlcron_jsonutil).
-export([
            to_json/1
        ]).

-include("urlcron.hrl").


to_json(ok) ->
    {struct, 
        [
            {<<"status">>, 1},
            {<<"data">>, <<"Successful">>}
        ]
    };


to_json({ok, Name}) when is_list(Name) ->
    {struct,
        [
            {<<"status">>, 1},
            {<<"name">>, list_to_binary(Name)}
        ]
    };


to_json({ok, Schedule}) when is_record(Schedule, schedule) ->
    {struct, 
        [
            {<<"status">>, 1},
            {<<"data">>, to_json(Schedule)}
        ]
    };


to_json({error, Reason}) when is_list(Reason) ->
    {struct, 
        [
            {<<"status">>, 0},
            {<<"data">>, list_to_binary(Reason)}
        ]
    };


to_json(Schedule) when is_record(Schedule, schedule) ->
    Name = Schedule#schedule.name,
    Pid = Schedule#schedule.pid,
    StartTime = Schedule#schedule.start_time,
    TimeCreated = Schedule#schedule.time_created,
    TimeStarted = Schedule#schedule.time_started,
    TimeCompleted = Schedule#schedule.time_completed,
    Url = Schedule#schedule.url,
    UrlStatus = Schedule#schedule.url_status,
    UrlHeaders = Schedule#schedule.url_headers,
    UrlContent = Schedule#schedule.url_content,
    status = Schedule#schedule.status,

    {struct, 
        [
            {<<"Name">>, list_to_binary(Name)},
            {<<"pid">>, to_json(Pid)},
            {<<"start_time">>, to_json(StartTime)},
            {<<"time_created">>, to_json(TimeCreated)},
            {<<"time_started">>, to_json(TimeStarted)},
            {<<"time_completed">>, to_json(TimeCompleted)},
            {<<"url">>, list_to_binary(Url)},
            {<<"url_status">>, list_to_binary(UrlStatus)},
            {<<"url_headers">>, list_to_binary(UrlHeaders)},
            {<<"url_content">>, list_to_binary(UrlContent)}
        ]
    };


to_json(Value) when is_list(Value) ->
    list_to_binary(Value);


to_json(Pid) when is_pid(Pid) ->
    List = pid_to_list(Pid),
    list_to_binary(List);


to_json({{Year, Month, Day}, {Hour, Minute, Seconds}}=StartTime) ->
    {struct,
        [
            {<<"Year">>, Year},
            {<<"Month">>, Month},
            {<<"Day">>, Day},
            {<<"Hour">>, Hour},
            {<<"Minute">>, Minute},
            {<<"Seconds">>, Seconds}
        ]                   
    };
    

to_json([Schedule | _Rest]=List) when is_record(Schedule, schedule)->
    Fun = fun(A) -> 
            to_json(A)
          end,
    lists:map(Fun, List).

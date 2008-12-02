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


to_json({ok, Data}) ->
    {struct,
        [
            {<<"status">>, 1},
            {<<"data">>, to_json(Data)}
        ]
    };


to_json({error, Data}) ->
    {struct, 
        [
            {<<"status">>, 0},
            {<<"data">>, to_json(Data)}
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
    Status = Schedule#schedule.status,

    {struct, 
        [
            {<<"name">>, to_json(Name)},
            {<<"pid">>, to_json(Pid)},
            {<<"start_time">>, to_json(StartTime)},
            {<<"time_created">>, to_json(TimeCreated)},
            {<<"time_started">>, to_json(TimeStarted)},
            {<<"time_completed">>, to_json(TimeCompleted)},
            {<<"url">>, to_json(Url)},
            {<<"url_status">>, to_json(UrlStatus)},
            {<<"url_headers">>, to_json(UrlHeaders)},
            {<<"url_content">>, to_json(UrlContent)},
            {<<"status">>, to_json(Status)}
        ]
    };


to_json(Pid) when is_pid(Pid) ->
    List = pid_to_list(Pid),
    list_to_binary(List);


to_json({{Year, Month, Day}, {Hour, Minute, Seconds}}) ->
    {struct,
        [
            {<<"year">>, Year},
            {<<"month">>, Month},
            {<<"day">>, Day},
            {<<"hour">>, Hour},
            {<<"minute">>, Minute},
            {<<"seconds">>, Seconds}
        ]                   
    };
    
to_json(Value) when is_list(Value) ->
    list_to_binary(Value);

to_json(Value) when is_atom(Value) ->
    List = atom_to_list(Value),
    list_to_binary(List);

to_json([Schedule | _Rest]=List) when is_record(Schedule, schedule)->
    Fun = fun(A) -> 
            to_json(A)
          end,
    lists:map(Fun, List).

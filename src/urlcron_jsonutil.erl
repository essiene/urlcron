-module(urlcron_jsonutil).
-export([
            json_response/1
        ]).


json_response(Response) ->
    case Response of 
        {ok, {Name, StartTime, Url}} ->
            {{Year, Month, Date}, {Hour, Min, Sec}} = StartTime,
            StrStartTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Date, Hour, Min, Sec]),            
            jsonify(1, Name, StrStartTime, Url);
        {ok, {Name, Details}} ->
            StrDetails = io_lib:format("~p", [Details]),
            StrName = io_lib:format("~p", [Name]),
            jsonify(1, StrName, StrDetails);
        {ok, Details} ->
            StrDetails = io_lib:format("~p", [Details]),
            jsonify(1, StrDetails);
        {error, Details} ->
            StrDetails = io_lib:format("~p", [Details]),
            jsonify(0, StrDetails);
        List ->
            Fun = fun(A) -> 
                    json_response(A)
                  end,
            lists:map(Fun, List)
    end.    


jsonify(Code, Name, StrStartTime, Url) ->
    {struct, 
        [
            {<<"status">>, Code},
            {<<"name">>, list_to_binary(Name)},
            {<<"starttime">>, list_to_binary(StrStartTime)},
            {<<"url">>, list_to_binary(Url)}
        ]
    }.

jsonify(Code, StrDetails) ->
    {struct, 
        [
            {<<"status">>, Code},
            {<<"details">>, list_to_binary(StrDetails)}
        ]
    }.

jsonify(Code, StrName, StrDetails) ->
    {struct, 
        [
            {<<"status">>, Code},
            {<<"name">>, list_to_binary(StrName)},
            {<<"details">>, list_to_binary(StrDetails)}
        ]
    }.
    

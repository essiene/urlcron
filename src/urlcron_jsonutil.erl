-module(urlcron_jsonutil).
-export([
            json_response/1
        ]).


json_response({ok, {Name, StartTime, Url}}) ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = StartTime,
    StrStartTime = io_lib:format("~p/~p/~p ~p:~p:~p", [Year, Month, Date, Hour, Min, Sec]),            
    jsonify(1, Name, StrStartTime, Url);

json_response({ok, {Name, Details}}) ->    
    jsonify(1, Name, Details);

json_response({ok, Details}) ->
    jsonify(1, Details);

json_response({error, Details}) ->
    StrDetails = io_lib:format("~p", [Details]),
    jsonify(0, StrDetails);

json_response(List) ->
    Fun = fun(A) -> 
            json_response(A)
          end,
    lists:map(Fun, List).


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
    

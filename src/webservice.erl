-module(webservice).
-export([
       get/2,
       post/2,
       delete/2,
       put/2
    ]).

get_value(Key, TupleList) ->
    case lists:keysearch(Key, 1, TupleList) of
        {value, {Key, Value}} ->
            Value;
        false ->
            []
    end.            

todate(QueryString) ->
    YY = erlang:list_to_integer(get_value("year", QueryString)),
    MM = erlang:list_to_integer(get_value("month", QueryString)),
    DD = erlang:list_to_integer(get_value("day", QueryString)),
    Hh = erlang:list_to_integer(get_value("hour", QueryString)),
    Mm = erlang:list_to_integer(get_value("minute", QueryString)),
    Ss = erlang:list_to_integer(get_value("seconds", QueryString)),
    {{YY, MM, DD}, {Hh, Mm, Ss}}.

post("/schedule", Req) ->
    QueryString = Req:parse_qs(),
    {StartTime, Url, Name} = get_basic_params(QueryString),
    RetVal = urlcron_scheduler:new(Name, StartTime, Url),
    Req:ok({"text/plain", io_lib:format("~p", [RetVal])}).

put("/schedule/" ++ Name, Req) ->
    QueryString = Req:parse_qs(),
    {_StartTime, _Url, _Name} = get_basic_params(QueryString),    
    %call update function
    Req:ok({"text/plain", io_lib:format("~p:~p", [Name, QueryString])}).

delete("/schedule/", Req) ->
    %call delete_all function
    Req:ok({"text/plain", io_lib:format("~p", [all])});

delete("/schedule/" ++ Name, Req) ->
    %call delete function
    Req:ok({"text/plain", io_lib:format("~p", [Name])}).

get("/schedule/", Req) ->
    QueryString = Req:parse_qs(),
    Format = get_value("format", QueryString),
    %call view all function
    Req:ok({"text/plain", io_lib:format("~p:~p", [all, Format])});

get("/schedule/" ++ Name, Req) ->
    QueryString = Req:parse_qs(),
    Format = get_value("format", QueryString),
    %call view function
    Req:ok({"text/plain", io_lib:format("~p:~p", [Name, Format])});

get("/stats", Req) ->
    %statistical data
    Req:ok({"text/plain", io_lib:format("~p", [stats])});

get("/", Req) ->
    get("", Req);

get("", Req) ->
    Req:ok({"text/plain", io_lib:format("Welcome to ~p", [urlcron])});

get(_Path, Req) ->
    Req:not_found().

get_basic_params(QueryString) ->
    StartTime = todate(QueryString),    
    Url = get_value("url", QueryString),
    Name = get_value("name", QueryString),
    {StartTime, Url, Name}.


-module(webservice).
-export([
       get/2
    ]).

get_value(Key, TupleList) ->
    case lists:keysearch(Key, 1, TupleList) of
        {value, {Key, Value}} ->
            Value;
        false ->
            ""
    end.            

todate(QueryString) ->
    YY = erlang:list_to_integer(get_value("YY", QueryString)),
    MM = erlang:list_to_integer(get_value("MM", QueryString)),
    DD = erlang:list_to_integer(get_value("DD", QueryString)),
    Hh = erlang:list_to_integer(get_value("hh", QueryString)),
    Mm = erlang:list_to_integer(get_value("mm", QueryString)),
    Ss = erlang:list_to_integer(get_value("ss", QueryString)),
    {{YY, MM, DD}, {Hh, Mm, Ss}}.

get("/new", Req) ->
    QueryString = Req:parse_qs(),
    StartTime = todate(QueryString),    
    Url = get_value("url", QueryString),
    Name = get_value("name", QueryString),
    RetVal = urlcron_scheduler:new(Name, StartTime, Url),
    Req:ok({"text/plain", io_lib:format("~p", [RetVal])});

get("/", Req) ->
    get("", Req);

get("", Req) ->
    QueryString = Req:parse_qs(),
    Req:ok({"text/plain", io_lib:format("~p", [QueryString])});

get(_Path, Req) ->
    Req:not_found().

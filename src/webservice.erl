-module(webservice).
-export([
       get/2,
       post/2,
       delete/2,
       put/2
    ]).

-export([
        create_new_schedule/1,
        view_schedule/0,
        view_schedule/1,
        delete_schedule/0,
        delete_schedule/1,
        update_schedule/2
    ]).


todate(QueryString) ->
    YY = erlang:list_to_integer(get_value("year", QueryString)),
    MM = erlang:list_to_integer(get_value("month", QueryString)),
    DD = erlang:list_to_integer(get_value("day", QueryString)),
    Hh = erlang:list_to_integer(get_value("hour", QueryString)),
    Mm = erlang:list_to_integer(get_value("minute", QueryString)),
    Ss = erlang:list_to_integer(get_value("seconds", QueryString)),
    {{YY, MM, DD}, {Hh, Mm, Ss}}.

post("/schedule", Req) ->
    QueryString = Req:parse_post(),
    Response = create_new_schedule(QueryString),
    Req:ok({"text/javascript", mochijson2:encode(Response)}).

put("/schedule/" ++ Name, Req) ->
    QueryString = Req:parse_qs(),
    {_StartTime, _Url, _Name} = get_basic_params(QueryString),    
    Response = update_schedule(Name, QueryString),
    Req:ok({"text/javascript", mochijson2:encode(Response)}).

delete("/schedule/", Req) ->
    Response = delete_schedule(),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

delete("/schedule/" ++ Name, Req) ->
    Response = delete_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)}).

get("/schedule/", Req) ->
    Response = view_schedule(),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

get("/schedule/" ++ Name, Req) ->
    Response = view_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

get("/stats/", Req) ->
    %statistical data
    Req:ok({"text/plain", io_lib:format("~p", [all_ok])});

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

get_value(Key, TupleList) ->
    case lists:keysearch(Key, 1, TupleList) of
        {value, {Key, Value}} ->
            Value;
        false ->
            []
    end.            


view_schedule() ->
    %Response = urlcron_scheduler:view_all()
    StartTime = {{2008, 12, 11}, {16, 12, 12}},
    Url = "http://localhost:8118",
    Name = "xmas",
    Response = [
                    {ok, {Name, StartTime, Url}},
                    {ok, {Name, StartTime, Url}},
                    {ok, {Name, StartTime, Url}},
                    {ok, {Name, StartTime, Url}}
               ],
    urlcron_jsonutil:json_response(Response).

view_schedule(Name) ->
    %Response = urlcron_scheduler:view_all()
    StartTime = {{2008, 12, 11}, {16, 12, 12}},
    Url = "http://localhost:8118",
    Response = {ok, {Name, StartTime, Url}},
    urlcron_jsonutil:json_response(Response).


delete_schedule() ->
    %Response = urlcron_scheduler:delete_all(),
    Response = {ok, "All Schedules Deleted"},
    urlcron_jsonutil:json_response(Response).

delete_schedule(Name) ->
    %Response = urlcron_scheduler:delete(Name),
    Response = {ok, {Name, "Deleted"}},
    urlcron_jsonutil:json_response(Response).


update_schedule(Name, QueryString) ->
    {StartTime, Url, _Name} = get_basic_params(QueryString),
    %Response = urlcron_scheduler:update(Name, StartTime, Url),
    Response = {ok, {Name, StartTime, Url}},
    urlcron_jsonutil:json_response(Response).


create_new_schedule(QueryString) ->
    {StartTime, Url, Name} = get_basic_params(QueryString),
    Response = urlcron_scheduler:new(Name, StartTime, Url),
    urlcron_jsonutil:json_response(Response).

-module(webservice).
-export([
       get/2,
       post/2,
       delete/2,
       put/2
    ]).

-export([
        create_new_schedule/1,
        get_schedule/1,
        delete_schedule/1,
        update_schedule/2,
        enable_schedule/1,
        disable_schedule/1
    ]).

-include("urlcron.hrl").


post("/schedule/", Req) ->
    post("/schedule", Req);

post("/schedule", Req) ->
    QueryString = Req:parse_post(),
    Response = create_new_schedule(QueryString),
    error_logger:info_report([new_schedule, {reponse, Response}]),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)}).


put("/schedule/" ++ Name, Req) ->    
    QueryString = Req:parse_post(),
    Response = update_schedule(Name, QueryString),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)}).

delete("/schedule/" ++ Name, Req) ->
    Response = delete_schedule(Name),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)}).


get("/schedule/enable/" ++ Name, Req) ->
    Response = enable_schedule(Name),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)});


get("/schedule/disable/" ++ Name, Req) ->
    Response = disable_schedule(Name),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)});


get("/schedule/" ++ Name, Req) ->
    Response = get_schedule(Name),
    error_logger:info_msg("~p~n", [Response]),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)});


get("/stats/", Req) ->
    %statistical data
    Req:ok({"text/plain", io_lib:format("~p", [all_ok])});


get("/echo/" ++ Name , Req) ->
    Req:ok({"text/plain", Name});


get("/", Req) ->
    get("", Req);


get("", Req) ->
    Req:ok({"text/plain", io_lib:format("Welcome to ~p", [urlcron])});


get(_Path, Req) ->
    Req:not_found().


enable_schedule(Name) ->
    urlcron_scheduler:enable(Name).


disable_schedule(Name) ->
    urlcron_scheduler:disable(Name).


get_schedule(Name) ->
    urlcron_scheduler:get(Name).


delete_schedule(Name) ->
    urlcron_scheduler:cancel(Name).


update_schedule(Name, QueryString) ->
    Url = get_value("url", QueryString),
    urlcron_scheduler:set(Name, url, Url).


create_new_schedule(QueryString) ->
    StartTime = get_datetime(QueryString),
    Url = get_value("url", QueryString),
    case proplists:get_value("name", QueryString) of
        undefined ->
            urlcron_scheduler:create(StartTime, Url);
        Name ->
            urlcron_scheduler:create(Name, StartTime, Url)
    end.

get_datetime(QueryString) ->
    Yyyy = erlang:list_to_integer(get_value("year", QueryString)),
    MM = erlang:list_to_integer(get_value("month", QueryString)),
    DD = erlang:list_to_integer(get_value("day", QueryString)),
    HH = erlang:list_to_integer(get_value("hour", QueryString)),
    Mm = erlang:list_to_integer(get_value("minute", QueryString)),
    Ss = erlang:list_to_integer(get_value("second", QueryString)),
    {{Yyyy, MM, DD}, {HH, Mm, Ss}}.

get_value(Key, QueryString) ->
    case proplists:get_value(Key, QueryString) of
        undefined ->
            throw({required_parameter_missing, Key});
        Value ->
            Value
    end.

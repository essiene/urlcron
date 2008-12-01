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

-include("urlcron.hrl").


post("/schedule", Req) ->
    QueryString = Req:parse_post(),
    Response = create_new_schedule(QueryString),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)}).

put("/schedule/" ++ Name, Req) ->    
    QueryString = Req:parse_post(),
    Response = update_schedule(Name, QueryString),
    Json = urlcron_jsonutil:to_json(Response),
    Req:ok({"text/javascript", mochijson2:encode(Json)}).

delete("/schedule/", Req) ->
    Response = delete_schedule(),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

delete("/schedule/" ++ Name, Req) ->
    Response = delete_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)}).

get("/schedule/", Req) ->
    Response = view_schedule(),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

get("/schedule/enable/" ++ Name, Req) ->
    Response = enable_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

get("/schedule/disable/" ++ Name, Req) ->
    Response = disable_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)});

get("/schedule/" ++ Name, Req) ->
    Response = view_schedule(Name),
    Req:ok({"text/javascript", mochijson2:encode(Response)});


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
    Response = urlcron_scheduler:enable(Name),
    urlcron_jsonutil:to_json(Response).


disable_schedule(Name) ->
    Response = urlcron_scheduler:disable(Name),
    urlcron_jsonutil:to_json(Response).


view_schedule() ->
    %Response = urlcron_scheduler:get_all()
    Name = "xmas",
    Response = [
                    {ok, Name},
                    {ok, Name},
                    {ok, Name},
                    {ok, Name}
               ],
    urlcron_jsonutil:to_json(Response).

view_schedule(Name) ->
    Response = urlcron_scheduler:get(Name),
    urlcron_jsonutil:to_json(Response).


delete_schedule() ->
    %Response = urlcron_scheduler:cancel_all(),
    Response = ok,
    urlcron_jsonutil:to_json(Response).

delete_schedule(Name) ->
    Response = urlcron_scheduler:cancel(Name),
    urlcron_jsonutil:to_json(Response).


update_schedule(Name, QueryString) ->
    {_StartTime, Url} = get_basic_params(QueryString),
    urlcron_scheduler:set(Name, url, Url).


create_new_schedule(QueryString) ->
    case get_basic_params(QueryString) of
        {StartTime, Url, Name} ->
            urlcron_scheduler:create(Name, StartTime, Url);
        {StartTime, Url} ->
            urlcron_scheduler:create(StartTime, Url)
    end.

todate(QueryString) ->
    YY = erlang:list_to_integer(get_value("year", QueryString)),
    MM = erlang:list_to_integer(get_value("month", QueryString)),
    DD = erlang:list_to_integer(get_value("day", QueryString)),
    Hh = erlang:list_to_integer(get_value("hour", QueryString)),
    Mm = erlang:list_to_integer(get_value("minute", QueryString)),
    Ss = erlang:list_to_integer(get_value("seconds", QueryString)),
    {{YY, MM, DD}, {Hh, Mm, Ss}}.

get_basic_params(QueryString) ->
    StartTime = todate(QueryString),    
    Url = get_value("url", QueryString),
    Name = get_value("name", QueryString),
    case Name of 
        [] ->
            {StartTime, Url};
        Value ->
            {StartTime, Url, Value}
    end.            

get_value(Key, TupleList) ->
    case lists:keysearch(Key, 1, TupleList) of
        {value, {Key, Value}} ->
            Value;
        false ->
            []
    end.            


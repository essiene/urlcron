-module(urlcron_util).
-export([
        get_datetime_diff/2,
        get_datetime_diff/1,
        get_future_time/1,
        gen_schedule_name/0,
        urlopen/1,
        ensure_started/1
       ]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

get_datetime_diff(Date) ->
    Now = calendar:now_to_local_time(erlang:now()),
    get_datetime_diff(Date, Now).

get_datetime_diff(DateB, DateA) ->
    Secs = fun(A) -> calendar:datetime_to_gregorian_seconds(A) end,
    DateASecs = Secs(DateA),
    DateBSecs = Secs(DateB),
    (DateBSecs - DateASecs) * 1000.

get_future_time(MilliSeconds) when is_integer(MilliSeconds) ->
    Seconds = MilliSeconds div 1000,
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Seconds).

gen_schedule_name() ->
    {_Mega, Secs, Milli} = erlang:now(),
    "schedule." ++ integer_to_list(Secs) ++ "." ++ integer_to_list(Milli).

urlopen(Url) -> 
    TimeStarted = urlcron_util:get_future_time(0),

    case http:request(Url) of 
        {error, Reason} ->
            Result = {error, undefined, Reason};
        {ok, {{_Version, Code, _Detail}, Headers, Content}} ->
            Result = {Code, Headers, Content}
    end,

    TimeCompleted = urlcron_util:get_future_time(0),

    {TimeStarted, TimeCompleted, Result}.

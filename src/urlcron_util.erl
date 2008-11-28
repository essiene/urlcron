-module(urlcron_util).
-export([
        get_datetime_diff/2,
        get_datetime_diff/1,
        get_future_time/1,
        gen_schedule_name/0
       ]).


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

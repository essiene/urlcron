-module(urlcron_util).
-export([
        date_diff/2,
        date_diff/1,
        time_from_now/1
       ]).


date_diff(Date) ->
    Now = calendar:now_to_local_time(erlang:now()),
    date_diff(Date, Now).

date_diff(DateB, DateA) ->
    Secs = fun(A) -> calendar:datetime_to_gregorian_seconds(A) end,
    DateASecs = Secs(DateA),
    DateBSecs = Secs(DateB),
    (DateBSecs - DateASecs) * 1000.

time_from_now(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Seconds).

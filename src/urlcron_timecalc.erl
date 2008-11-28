-module(urlcron_timecalc).

-export([
        date_diff/2,
        date_diff/1
       ]).


date_diff(Date) ->
    Now = calendar:now_to_local_time(erlang:now()),
    date_diff(Date, Now).



date_diff(DateB, DateA) ->
    Secs = fun(A) -> calendar:datetime_to_gregorian_seconds(A) end,
    DateASecs = Secs(DateA),
    DateBSecs = Secs(DateB),
    (DateBSecs - DateASecs) * 1000.

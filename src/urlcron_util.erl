-module(urlcron_util).
-export([
        time_from_now/1
    ]).


time_from_now(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + Seconds).

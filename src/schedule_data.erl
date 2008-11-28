-module(schedule_data).
-include("urlcron.hrl").
-export([
        new/2,
        new/3
    ]).

new(StartTime, Url) ->
    new(StartTime, Url, none).

new({Date, Time}=StartTime, Url, none) when is_list(Url), is_tuple(Date), is_tuple(Time) ->
    #schedule_data{
        start_time = StartTime,
        url = Url,
        timer = none
    };

new({Date, Time}=StartTime, Url, Timer) when is_reference(Timer), is_list(Url), is_tuple(Date), is_tuple(Time) ->
    #schedule_data{
        start_time = StartTime,
        url = Url,
        timer = Timer
    }.

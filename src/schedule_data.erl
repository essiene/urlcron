-module(schedule_data).
-include("urlcron.hrl").
-export([
        new/2
    ]).

new(Name, Timer) when is_reference(Timer), is_list(Name) ->
    #schedule_data{
        name = Name,
        timer = Timer
    }.

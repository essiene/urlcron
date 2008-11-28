-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").


new_anonymous_schedule_test() ->
    urlcron_scheduler:start(),
    Starttime = urlcron_util:time_from_now(60),
    Url = "url",
    {ok, {_Name, Starttime, Url}} = urlcron_scheduler:new(Starttime, Url),
    urlcron_scheduler:stop().

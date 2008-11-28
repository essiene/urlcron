-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").


new_anonymous_schedule_test() ->
    urlcron_scheduler:start(),
    Starttime = urlcron_util:get_future_time(60000),
    Url = "url",
    {ok, {_Name, Starttime, Url}} = urlcron_scheduler:new(Starttime, Url),
    urlcron_scheduler:stop().

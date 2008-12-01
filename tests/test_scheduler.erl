-module(test_scheduler).
-include_lib("eunit/include/eunit.hrl").
-include("urlcron.hrl").


new_test() ->
    Config = erlcfg:new("urlcron.conf"),
    urlcron_mochiweb:start(Config),
    urlcron_scheduler:start(Config).

new_named_schedule_test() ->
    Starttime = urlcron_util:get_future_time(60000),
    ?assertEqual({ok, "schedule01"}, urlcron_scheduler:create("schedule01", Starttime, "url")),

    {ok, Schedule} = urlcron_scheduler:get("schedule01"),
    ?assertEqual(true, is_process_alive(Schedule#schedule.pid)).

new_anonymous_schedule_test() ->
    Starttime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(Starttime, "url"),
    ?assertEqual(1, string:str(Name, "schedule.")).

get_test() ->
    StartTime = urlcron_util:get_future_time(6000),
    {ok, "schedule02"} = urlcron_scheduler:create("schedule02", StartTime, "http://boo.com"),

    {ok, #schedule{name=Name, start_time=StartTime1, url=Url, status=Status}} = urlcron_scheduler:get("schedule02"),
    Expected = {"schedule02", StartTime, "http://boo.com", enabled},
    ?assertEqual(Expected, {Name, StartTime1, Url, Status}).

get_not_found_test() ->
    Result = urlcron_scheduler:get("schedule03"),
    Expected = {error, not_found},
    ?assertEqual(Expected, Result).

set_url_test() ->
    ?assertEqual(ok, urlcron_scheduler:set("schedule01", url, "google.com")).

set_url_not_found_test() ->
    ?assertEqual({error, not_found}, urlcron_scheduler:set("schedule03", url, "google.com")).

set_url_on_completed_test() ->
    StartTime = urlcron_util:get_future_time(1000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    timer:sleep(2000),

    ?assertEqual({error, schedule_already_completed}, urlcron_scheduler:set(Name, url, "google.com")).

disable_enabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "google.com"),

    ?assertEqual(ok, urlcron_scheduler:disable(Name)),
    {ok, Schedule} = urlcron_scheduler:get(Name),
    ?assertEqual(undefined, Schedule#schedule.pid),
    ?assertEqual(disabled, Schedule#schedule.status).

disable_disabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "google.com"),

    ?assertEqual(ok, urlcron_scheduler:disable(Name)),
    ?assertEqual(ok, urlcron_scheduler:disable(Name)).

disable_completed_test() ->
    StartTime = urlcron_util:get_future_time(1000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    timer:sleep(2000),

    ?assertEqual({error, schedule_already_completed}, urlcron_scheduler:disable(Name)).

enable_disabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "google.com"),

    ?assertEqual(ok, urlcron_scheduler:disable(Name)),
    ?assertEqual(ok, urlcron_scheduler:enable(Name)),

    {ok, Schedule} = urlcron_scheduler:get(Name),
    ?assertEqual(true, is_process_alive(Schedule#schedule.pid)),
    ?assertEqual(enabled, Schedule#schedule.status).

enable_enabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "google.com"),

    ?assertEqual(ok, urlcron_scheduler:disable(Name)),
    ?assertEqual(ok, urlcron_scheduler:enable(Name)),
    ?assertEqual(ok, urlcron_scheduler:enable(Name)).

enabled_completed_test() ->
    StartTime = urlcron_util:get_future_time(1000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    timer:sleep(2000),

    ?assertEqual({error, schedule_already_completed}, urlcron_scheduler:enable(Name)).

cancel_enabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    {ok, Schedule} = urlcron_scheduler:get(Name),

    ?assertEqual(ok, urlcron_scheduler:cancel(Name)),
    ?assertEqual(false, is_process_alive(Schedule#schedule.pid)),
    ?assertEqual({error, not_found}, urlcron_scheduler:get(Name)).

cancel_disabled_test() ->
    StartTime = urlcron_util:get_future_time(60000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    ok = urlcron_scheduler:disable(Name),

    ?assertEqual(ok, urlcron_scheduler:cancel(Name)),
    ?assertEqual({error, not_found}, urlcron_scheduler:get(Name)).

cancel_completed_test() ->
    StartTime = urlcron_util:get_future_time(1000),
    {ok, Name} = urlcron_scheduler:create(StartTime, "http://localhost:8118/echo/fetch_url_test"),

    timer:sleep(2000),

    ?assertEqual(ok, urlcron_scheduler:cancel(Name)),
    ?assertEqual({error, not_found}, urlcron_scheduler:get(Name)).




destroy_test() ->
    Config = erlcfg:new("urlcron.conf"),
    urlcron_scheduler:stop(),
    urlcron_mochiweb:stop(),
    schedule_store:destroy(Config).

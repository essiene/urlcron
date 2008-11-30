-define(SCHEDULER, urlcron_scheduler).

-record(schedule_data, {
        name,
        timer
        }).

% Mnesia Table For Schedules

-record(schedule, {
        name, % unique
        process = undefined,
        start_time = undefined,
        time_created = urlcron_util:get_future_time(0),
        time_started = undefined,
        time_completed = undefined,
        url = undefined,
        fetch_status = undefined,
        http_status = undefined,
        http_headers = undefined,
        http_content = undefined,
        status = disabled % enabled | disabled | active | completed
        }).

-define(SCHEDULER, urlcron_scheduler).

-record(schedule_data, {
        start_time,
        url,
        timer
        }).

% Mnesia Table For Schedules

-record(schedule, {
        name, % unique
        process,
        start_time,
        time_created,
        time_started,
        time_completed,
        url,
        fetch_status,
        http_status,
        http_headers,
        http_content,
        timer,
        status % enabled | disabled | active | completed
        }).

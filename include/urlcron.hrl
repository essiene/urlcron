-define(SCHEDULER, urlcron_scheduler).

-record(schedule_data, {
        start_time,
        url,
        timer
        }).

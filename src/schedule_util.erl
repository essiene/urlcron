-module(schedule_util).
-export([
        create/3,
        cancel/1,
        get/1,
        update/2,
        enable/1,
        disable/1,
        if_found_schedule/2
    ]).

-export([
        start/0,
        start/1,
        stop/0,
        stop/1
    ]).

-include("urlcron.hrl").

if_found_schedule(Name, Fun) ->
    case schedule_store:get(Name) of 
        {error, not_found}=Response ->
            Response;
        Schedule ->
            Fun(Schedule)
    end.

create(Name, StartTime, Url) ->
    case schedule_store:get(Name) of
        {error, not_found} ->
            case urlcron_schedule:start(Name, StartTime) of
                {error, _Reason} = Reply ->
                    Reply;
                {ok, Pid} ->
                    case schedule_store:add(Name, Pid, StartTime, Url, enabled) of
                        {error, _Reason} = Reply1 ->
                            Reply1;
                        ok ->
                            {ok, Name}
                    end
            end;
        _Schedule ->
            {error, already_exists}
    end.

cancel(Name) ->
    Found = fun
        (#schedule{status=enabled}=Schedule) ->
            urlcron_schedule:stop(Schedule#schedule.pid),
            schedule_store:delete(Name);
        (_Schedule) ->
            schedule_store:delete(Name)
    end,
    if_found_schedule(Name, Found).

get(Name) ->
    Found = fun(Schedule) -> 
            {ok, Schedule}
    end,
    if_found_schedule(Name, Found).

update(Name, ValueList) ->
    case schedule_store:update(Name, ValueList) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            ok
    end.


enable(Name) ->
    Found = fun
        (#schedule{status=completed}) ->
            {error, schedule_already_completed};
        (#schedule{status=enabled}) ->
            ok;
        (#schedule{status=disabled}=Schedule) ->
            start(Schedule)
    end,
    if_found_schedule(Name, Found).

disable(Name) ->
    Found = fun
        (#schedule{status=completed}) ->
            {error, schedule_already_completed};
        (#schedule{status=enabled}=Schedule) ->
            NewSchedule = Schedule#schedule{status=disabled},
            stop(NewSchedule);
        (#schedule{status=disabled}) ->
            ok
    end,

    if_found_schedule(Name, Found).

start() ->
    Fun = fun(Schedule) ->
            start(Schedule)
    end,

    case schedule_store:get_enabled() of
        {error, Reason} ->
            throw(Reason);
        Schedule ->
            lists:foreach(Fun, Schedule)
    end.

start(#schedule{name=Name, start_time=StartTime}=Schedule) ->
    case urlcron_schedule:start(Name, StartTime) of
        {error, Reason} = Reply->
            TimeStarted = urlcron_util:get_future_time(0),
            UpdatedSchedule = Schedule#schedule{
                status=completed, 
                pid = undefined,
                time_started=TimeStarted, 
                time_completed=TimeStarted,
                url_status = error,
                url_headers = undefined,
                url_content = Reason 
            },
            schedule_store:update(UpdatedSchedule),
            Reply;
        {ok, Pid} ->
            NewSchedule = Schedule#schedule{status=enabled, pid=Pid},
            case schedule_store:update(NewSchedule) of
                {error, _Reason} ->
                    %log
                    void;
                ok ->
                    void
            end,
            ok
    end.


stop() ->
    Fun = fun(Schedule) ->
            stop(Schedule)
    end,

    case schedule_store:get_enabled() of
        {error, _Reason} ->
            %log
            void;
        Schedule ->
            lists:foreach(Fun, Schedule)
    end.

stop(Schedule) ->
    urlcron_schedule:stop(Schedule#schedule.pid),
    NewSchedule = Schedule#schedule{pid=undefined},
    schedule_store:update(NewSchedule).




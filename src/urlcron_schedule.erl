-module(urlcron_schedule).
-behaviour(gen_fsm).

-export([
        start/2,
        stop/1,
        get_timer/1
    ]).

-export([
        running/2,
        running/3
    ]).

-export([
        init/1,
        handle_sync_event/4,
        handle_event/3,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-include("urlcron.hrl").

% public api
start(Name, StartTime) ->
    gen_fsm:start(?MODULE, [Name, StartTime], []).

%update these apis to use Name, instead of the schedule PID
get_timer(Schedule) ->
    gen_fsm:sync_send_all_state_event(Schedule, get_timer).

stop(Schedule) ->
    gen_fsm:send_all_state_event(Schedule, stop).

fetch_url(Name) ->
    case schedule_store:get(Name) of
        {error, Reason} ->
            throw({getting_schedule, Name, Reason});
        Schedule = #schedule{status=enabled, url=Url} ->
            {TimeStarted, TimeCompleted, {Code, Headers, Content}} = urlcron_util:urlopen(Url),
            UpdatedSchedule = Schedule#schedule{
                status=completed, 
                pid = undefined,
                time_started=TimeStarted, 
                time_completed=TimeCompleted,
                url_status = Code,
                url_headers = Headers,
                url_content = Content
            },
            schedule_store:update(UpdatedSchedule);
        #schedule{status=Status} ->
            throw({invalid_schedule_status, Status})
    end.


% gen_fsm states callbacks

running(wakeup, #schedule_data{name=Name}=State) ->
    fetch_url(Name),
    {stop, normal, State};

running(_Request, State) ->
    {nextstate, running, State}.

running(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, running, State}.

% Generic gen_fsm callbacks
init([Name, StartTime]) ->
    error_logger:info_report([new_running_schedule, {name, Name}, {starttime, StartTime}]),

    case urlcron_util:get_datetime_diff(StartTime) of
        MilliSecs when MilliSecs > 999 ->
            case gen_fsm:send_event_after(MilliSecs, wakeup) of
                {error, Reason} ->
                    {stop, Reason};
                TimerRef ->
                    Data = schedule_data:new(Name, TimerRef),
                    {ok, running, Data}
            end;
        _MilliSecs ->
            {stop, schedule_will_never_run}
    end.

handle_sync_event(get_timer, _From, StateName, #schedule_data{timer=Timer}=State) ->
    {reply, Timer, StateName, State};

handle_sync_event(Request, _From, StateName, State) ->
    {reply, {error, {illegal_request, Request}}, StateName, State}.



handle_event(stop, _StateName, State) ->
    {stop, normal, State};

handle_event(_Request, StateName, State) ->
    {nextstate, StateName, State}.

handle_info(_Info, StateName, State) ->
    {nextstate, StateName, State}.




terminate(_Reason, _StateName, #schedule_data{timer=Timer}) ->
    gen_fsm:cancel_timer(Timer),
    ok.



code_change(_OldVsn, StateName, State, _Extra) ->
    {nextstate, StateName, State}.

-module(urlcron_schedule).
-behaviour(gen_fsm).

-export([
        start/3,
        start_link/3,
        stop/1,
        get_timer/1,
        get_status/1
    ]).

-export([
        inactive_enabled/2,
        inactive_enabled/3,

        inactive_disabled/2,
        inactive_disabled/3
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
start(StartTime, Url, Flag) ->
    gen_fsm:start(?MODULE, [StartTime, Url, Flag], []).

start_link(StartTime, Url, Flag) ->
    gen_fsm:start_link(?MODULE, [StartTime, Url, Flag], []).

get_timer(Schedule) ->
    gen_fsm:sync_send_all_state_event(Schedule, get_timer).

get_status(Schedule) ->
    gen_fsm:sync_send_all_state_event(Schedule, get_status).

stop(Schedule) ->
    gen_fsm:send_all_state_event(Schedule, stop).

% gen_fsm states callbacks

inactive_enabled(wakeup, #schedule_data{url=Url}=State) ->
    error_logger:info_msg("Waking up to call: ~s", [Url]),
    {_Status, _Detail} = http:request(url),
    {stop, normal, State};

inactive_enabled(_Request, State) ->
    {nextstate, inactive_enabled, State}.

inactive_enabled(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, inactive_enabled, State}.

inactive_disabled(wakeup, State) ->
    {nextstate, inactive_disabled, State};

inactive_disabled(_Request, State) ->
    {nextstate, inactive_disabled, State}.

inactive_disabled(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, inactive_disabled, State}.

% Generic gen_fsm callbacks
init([StartTime, Url, enabled]) ->
    MilliSecs = urlcron_util:get_datetime_diff(StartTime),
    TimerRef = gen_fsm:send_event_after(MilliSecs, wakeup),
    Data = schedule_data:new(StartTime, Url, TimerRef),

    error_logger:info_msg("New schedule created ~p", [Data]),
    {ok, inactive_enabled, Data};

init([StartTime, Url, disabled]) ->
    Data = schedule_data:new(StartTime, Url),
    error_logger:info_msg("New schedule created ~p", [Data]),
    {ok, inactive_disabled, Data}.



handle_sync_event(get_timer, _From, StateName, #schedule_data{timer=Timer}=State) ->
    {reply, Timer, StateName, State};

handle_sync_event(get_status, _From, StateName, #schedule_data{start_time=StartTime, url=Url, timer=Timer}=State) ->
    Status = {StartTime, Url, Timer, StateName},
    {reply, Status, StateName, State};

handle_sync_event(Request, _From, StateName, State) ->
    {reply, {error, {illegal_request, Request}}, StateName, State}.



handle_event(stop, _StateName, State) ->
    {stop, normal, State};

handle_event(_Request, StateName, State) ->
    {nextstate, StateName, State}.




handle_info(_Info, StateName, State) ->
    {nextstate, StateName, State}.




terminate(_Reason, _StateName, _State) ->
    ok.



code_change(_OldVsn, StateName, State, _Extra) ->
    {nextstate, StateName, State}.

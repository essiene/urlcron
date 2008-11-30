-module(urlcron_schedule).
-behaviour(gen_fsm).

-export([
        start/2,
        start_link/2,
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

start_link(Name, StartTime) ->
    gen_fsm:start_link(?MODULE, [Name, StartTime], []).

get_timer(Schedule) ->
    gen_fsm:sync_send_all_state_event(Schedule, get_timer).

stop(Schedule) ->
    gen_fsm:send_all_state_event(Schedule, stop).

% gen_fsm states callbacks

running(wakeup, #schedule_data{name=_Name}=State) ->
%    error_logger:info_msg("Waking up to call: ~s", [Url]),
%    {_Status, _Detail} = http:request(Url),
    {stop, normal, State};

running(_Request, State) ->
    {nextstate, running, State}.

running(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, running, State}.

% Generic gen_fsm callbacks
init([Name, StartTime]) ->
    MilliSecs = urlcron_util:get_datetime_diff(StartTime),
    TimerRef = gen_fsm:send_event_after(MilliSecs, wakeup),
    Data = schedule_data:new(Name, TimerRef),
    {ok, running, Data}.

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




terminate(_Reason, _StateName, _State) ->
    ok.



code_change(_OldVsn, StateName, State, _Extra) ->
    {nextstate, StateName, State}.

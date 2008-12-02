-module(urlcron_scheduler).
-behaviour(gen_server).
-export([
        start/0,
        start/1,
        start_link/1,
        stop/0
    ]).

-export([
        create/2,
        create/3,
        get/1,
        set/3,
        enable/1,
        disable/1,
        cancel/1
    ]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-include("urlcron.hrl").

% Schedule management tasks

create(Starttime, Url) ->
    Name = urlcron_util:gen_schedule_name(),
    create(Name, Starttime, Url).

create(Name, Starttime, Url) ->
    gen_server:call(?SCHEDULER, {create, Name, Starttime, Url}).

get(Name) ->
    gen_server:call(?SCHEDULER, {get, Name}).

set(Name, Key, Value) ->
    gen_server:call(?SCHEDULER, {set, Name, Key, Value}).

enable(Name) ->
    gen_server:call(?SCHEDULER, {enable, Name}).

disable(Name) ->
    gen_server:call(?SCHEDULER, {disable, Name}).

cancel(Name) ->
    gen_server:call(?SCHEDULER, {cancel, Name}).

% Server management tasks

start() ->
    start(erlcfg:new()).

start(Config) ->
    gen_server:start({local, ?SCHEDULER}, ?MODULE, [Config], []).

start_link(Config) ->
    gen_server:start_link({local, ?SCHEDULER}, ?MODULE, [Config], []).

stop() ->
    gen_server:cast(?SCHEDULER, stop).


% Gen server callbacks

init([Config]) ->
    case inets:start() of
        ok ->
            ok;
        {error, {already_started, inets}} ->
            ok;
        Other ->
            throw(Other)
    end,

    schedule_store:start(Config),
    error_logger:info_msg("~p: Started~n", [?MODULE]),
    {ok, nil}.

handle_call({create, Name, StartTime, Url}, _From, State) ->
    Reply = start_schedule_and_store(Name, StartTime, Url),
    {reply, Reply, State};


handle_call({get, Name}, _From, State) ->
    Found = fun(Schedule) -> 
            {reply, {ok, Schedule}, State}
    end,

    if_found_schedule({Name, State}, Found);

handle_call({set, Name, url, Url}, _From, State) ->
    case schedule_store:update(Name, [{url, Url}]) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        ok ->
            {reply, ok, State}
    end;

handle_call({enable, Name}, _From, State) ->
    Found = fun
        (#schedule{status=completed}) ->
            {reply, {error, schedule_already_completed}, State};
        (#schedule{status=enabled}) ->
            {reply, ok, State};
        (#schedule{status=disabled}=Schedule) ->
            Reply = start_schedule_and_update(Schedule),
            {reply, Reply, State}
    end,

    if_found_schedule({Name, State}, Found);


handle_call({disable, Name}, _From, State) ->
    Found = fun
        (#schedule{status=completed}) ->
            {reply, {error, schedule_already_completed}, State};
        (#schedule{status=enabled}=Schedule) ->
            NewSchedule = Schedule#schedule{status=disabled},
            Reply = stop_schedule_and_update(NewSchedule),
            {reply, Reply, State};
        (#schedule{status=disabled}) ->
            {reply, ok, State}
    end,

    if_found_schedule({Name, State}, Found);


handle_call({cancel, Name}, _From, State) ->
    Found = fun
        (#schedule{status=enabled}=Schedule) ->
            urlcron_schedule:stop(Schedule#schedule.pid),
            Reply = schedule_store:delete(Name),
            {reply, Reply, State};
        (_Schedule) ->
            Reply = schedule_store:delete(Name),
            {reply, Reply, State}
    end,

    if_found_schedule({Name, State}, Found);
            

handle_call(Request, _From, State) ->
    {reply, {error, {illegal_request, Request}}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    schedule_store:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.


if_found_schedule({Name, State}, Fun) ->
    case schedule_store:get(Name) of 
        {error, not_found} ->
            {reply, {error, not_found}, State};
        Schedule ->
            Fun(Schedule)
    end.

stop_schedule_and_update(Schedule) ->
    urlcron_schedule:stop(Schedule#schedule.pid),
    NewSchedule = Schedule#schedule{pid=undefined},
    Reply = schedule_store:update(NewSchedule),
    Reply.

start_schedule_and_update(#schedule{name=Name, start_time=StartTime}=Schedule) ->
    case urlcron_schedule:start(Name, StartTime) of
        {error, _Reason} = Reply->
            Reply;
        {ok, Pid} ->
            NewSchedule = Schedule#schedule{status=enabled, pid=Pid},
            Reply = schedule_store:update(NewSchedule),
            Reply
    end.

start_schedule_and_store(Name, StartTime, Url) ->
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
    end.

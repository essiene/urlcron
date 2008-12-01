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
    case urlcron_schedule:start(Name, StartTime) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        {ok, Pid} ->
            case schedule_store:add(Name, Pid, StartTime, Url, enabled) of
                {error, Reason} ->
                    {reply, {error, Reason}, State};
                ok ->
                    {reply, {ok, Name}, State}
            end
    end;

handle_call({get, Name}, _From, State) ->
    case schedule_store:get(Name) of
        {error, not_found} ->
            {reply, {error, not_found}, State};
        Schedule ->
            {reply, {ok, Schedule}, State}
    end;

handle_call({set, Name, url, Url}, _From, State) ->
    case schedule_store:update(Name, [{url, Url}]) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        ok ->
            {reply, ok, State}
    end;

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

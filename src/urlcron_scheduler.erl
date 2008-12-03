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
    urlcron_util:ensure_started(inets),
    schedule_store:start(Config),

    schedule_util:start(),

    error_logger:info_msg("~p: Started~n", [?MODULE]),

    {ok, nil}.

handle_call({create, Name, StartTime, Url}, _From, State) ->
    Reply = schedule_util:create(Name, StartTime, Url),
    {reply, Reply, State};

handle_call({cancel, Name}, _From, State) ->
    Reply = schedule_util:cancel(Name),
    {reply, Reply, State};

handle_call({get, Name}, _From, State) ->
    Reply = schedule_util:get(Name),
    {reply, Reply, State};


handle_call({set, Name, url, Url}, _From, State) ->
    Reply = schedule_util:update(Name, [{url, Url}]),
    {reply, Reply, State};


handle_call({enable, Name}, _From, State) ->
    Reply = schedule_util:enable(Name),
    {reply, Reply, State};

handle_call({disable, Name}, _From, State) ->
    Reply = schedule_util:disable(Name),
    {reply, Reply, State};


handle_call(Request, _From, State) ->
    {reply, {error, {illegal_request, Request}}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    schedule_util:stop(),
    schedule_store:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

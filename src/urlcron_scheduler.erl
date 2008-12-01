-module(urlcron_scheduler).
-behaviour(gen_server).
-export([
        start/0,
        start/1,
        start_link/1,
        stop/0,
        new/2,
        new/3,
        get_status/1
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

% Public API

start() ->
    start(erlcfg:new()).

start(Config) ->
    gen_server:start({local, ?SCHEDULER}, ?MODULE, [Config], []).

start_link(Config) ->
    gen_server:start_link({local, ?SCHEDULER}, ?MODULE, [Config], []).

stop() ->
    gen_server:cast(?SCHEDULER, stop).

new(Starttime, Url) ->
    Name = urlcron_util:gen_schedule_name(),
    new(Name, Starttime, Url).

new(Name, Starttime, Url) ->
    gen_server:call(?SCHEDULER, {new, Name, Starttime, Url}).

get_status(Name) ->
    gen_server:call(?SCHEDULER, {status, Name}).


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

handle_call({new, Name, StartTime, Url}, _From, State) ->
    case urlcron_schedule:start(Name, StartTime) of
        {ok, Pid} ->
            case schedule_store:add(Name, Pid, StartTime, Url, enabled) of
                {error, Reason} ->
                    Reply = {error, Reason};
                ok ->
                    Reply = {ok, {Name, StartTime, Url}}
            end;
        {error, Reason} ->
            Reply = {error, Reason}
    end,

    {reply, Reply, State};

handle_call({status, Name}, _From, State) ->
    case schedule_store:get(Name) of
        {error, not_found} ->
            Reply = {error, not_found};
        #schedule{name=Name, start_time=StartTime, url=Url, status=Status} ->
            Reply = {Name, StartTime, Url, Status}
    end,
        
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
    schedule_store:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

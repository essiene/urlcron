-module(urlcron_scheduler).
-behaviour(gen_server).
-export([
        start/0,
        start_link/1,
        stop/0,
        new/2,
        new/3
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
    gen_server:start({local, ?SCHEDULER}, ?MODULE, [], []).

start_link(_Config) ->
    gen_server:start_link({local, ?SCHEDULER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SCHEDULER, stop).

new(Starttime, Url) ->
    Name = urlcron_util:gen_schedule_name(),
    new(Name, Starttime, Url).

new(Name, Starttime, Url) ->
    gen_server:call(?SCHEDULER, {new, Name, Starttime, Url}).


% Gen server callbacks

init([]) ->
    case inets:start() of
        ok ->
            ok;
        {error, {already_started, inets}} ->
            ok;
        Other ->
            Other
    end,

    error_logger:info_msg("~p: Started~n", [?MODULE]),
    {ok, gb_trees:empty()}.

handle_call({new, Name, StartTime, Url}, _From, State) ->
    case urlcron_schedule:start(StartTime, Url, enabled) of
        {ok, Pid} ->
            NewState = gb_trees:enter(Name, Pid, State),
            Reply = {ok, {Name, StartTime, Url}};
        {error, Reason} ->
            NewState = State,
            Reply = {error, Reason}
    end,

    {reply, Reply, NewState};

handle_call(Request, _From, State) ->
    {reply, {error, {illegal_request, Request}}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

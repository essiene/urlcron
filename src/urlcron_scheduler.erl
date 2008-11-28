-module(urlcron_scheduler).
-behaviour(gen_server).
-export([
        start/0,
        start_link/1,
        stop/0,
        new/2
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
    gen_server:call(?SCHEDULER, {new, Starttime, Url}).


% Gen server callbacks

init([]) ->
    error_logger:info_msg("~p: Started~n", [?MODULE]),
    {ok, []}.

handle_call({new, StartTime, Url}, _From, State) ->
    %create new fsm here assume we get a response
    {reply, {ok, autoname, {StartTime, Url}}, State};

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

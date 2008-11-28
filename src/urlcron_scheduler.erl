-module(urlcron_scheduler).
-behaviour(gen_server).
-export([
        start_link/0
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


start_link() ->
    gen_server:start_link(?SCHEDULER, ?MODULE, [], []).


init([]) ->
    {ok, []}.

handle_call(Request, _From, State) ->
    {reply, {error, {illegal_request, Request}}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

-module(urlcron_schedule).
-behaviour(gen_fsm).

-export([
    ]).

-export([
        inactive_enabled/2,
        inactive_enabled/3,

        inactive_disabled/2,
        inactive_disabled/3,

        active/2,
        active/3,

        completed/2,
        completed/3
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

% gen_fsm states callbacks

inactive_enabled(_Request, State) ->
    {nextstate, inactive_enabled, State}.

inactive_enabled(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, inactive_enabled, State}.

inactive_disabled(_Request, State) ->
    {nextstate, inactive_disabled, State}.

inactive_disabled(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, inactive_disabled, State}.

active(_Request, State) ->
    {nextstate, active, State}.

active(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, active, State}.

completed(_Request, State) ->
    {nextstate, completed, State}.

completed(Request, _From, State) ->
    {reply, {error, {illegal_Request, Request}}, completed, State}.

% Generic gen_fsm callbacks

init([StartTime, Url]) ->
    {ok, inactive_enabled, {StartTime, Url}}.


handle_sync_event(Request, _From, StateName, State) ->
    {reply, {error, {illegal_request, Request}}, StateName, State}.

handle_event(_Request, StateName, State) ->
    {nextstate, StateName, State}.

handle_info(_Info, StateName, State) ->
    {nextstate, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {nextstate, StateName, State}.


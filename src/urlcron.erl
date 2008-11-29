%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(urlcron).
-author('author <author@example.com>').
-export([start/0, status/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the urlcron server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    application:start(urlcron).

status() ->
    [{webservice, "Alive"}, {scheduler, "Alive"}].

%% @spec stop() -> ok
%% @doc Stop the urlcron server.
stop() ->
    Res = application:stop(urlcron),
    application:stop(inets),
    application:stop(crypto),
    Res.

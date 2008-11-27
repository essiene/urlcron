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
    application:start(urlcron).

status() ->
    [{pqueueserv, "Alive"}, {callserv, "Alive"}, {chanserv, "Alive"}].

%% @spec stop() -> ok
%% @doc Stop the urlcron server.
stop() ->
    Res = application:stop(urlcron),
    application:stop(crypto),
    Res.

%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(urlcron).
-author('author <author@example.com>').
-export([start/0, status/0, stop/0]).

        
%% @spec start() -> ok
%% @doc Start the urlcron server.
start() ->
    urlcron_util:ensure_started(crypto),
    urlcron_util:ensure_started(inets),
    application:start(urlcron).

status() ->
    [
        {webservice, urlcron_mochiweb:ping()}, 
        {scheduler, urlcron_scheduler:ping()}
    ].

%% @spec stop() -> ok
%% @doc Stop the urlcron server.
stop() ->
    Res = application:stop(urlcron),
    application:stop(inets),
    application:stop(crypto),
    Res.

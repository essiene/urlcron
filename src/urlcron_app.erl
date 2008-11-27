%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the urlcron application.

-module(urlcron_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for urlcron.
start(_Type, StartArgs) ->
    urlcron_sup:start_link(StartArgs).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for urlcron.
stop(_State) ->
    ok.

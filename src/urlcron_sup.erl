%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the urlcron application.

-module(urlcron_sup).
-author('author <author@example.com>').
-include("urlcron.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/1, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Config = case erlcfg:new("/etc/urlcron.conf") of
        {error, Reason} ->
            throw(Reason);
        Result ->
            Result
    end,


    Web = {urlcron_mochiweb, 
        {urlcron_mochiweb, start, [Config]}, 
        permanent, 5000, worker, dynamic},

    Scheduler = {?SCHEDULER,
        {urlcron_scheduler, start_link, [Config]},
        permanent, 5000, worker, [urlcron_scheduler]},

    Processes = [Scheduler, Web],

    {ok, {{one_for_one, 10, 10}, Processes}}.

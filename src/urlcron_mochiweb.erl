-module(urlcron_mochiweb).
-export([start/0, start/1, stop/0, loop/1]).

%% External API

start() ->
    start([
            {listen, "0.0.0.0"}, 
            {port, 8118}
        ]).

start(Config) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
           mochiweb_http:start([
                   {name, ?MODULE}, 
                   {loop, Loop}, 
                   {ip, urlcron_config:get(Config, listen)},
                   {port, urlcron_config:get(Config, port)}
               ]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Method = Req:get(method),
    Method1 = atom_to_list(Method),
    Method2 = string:to_lower(Method1),
    Method3 = list_to_atom(Method2),
    apply(webservice, Method3, [Req:get(path), Req]).

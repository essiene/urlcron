-module(urlcron_mochiweb).
-export([start/1, stop/0, loop/1]).

%% External API

start(Config) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
           mochiweb_http:start([
                   {name, ?MODULE}, 
                   {loop, Loop}, 
                   {ip, Config:get(server.listen)},
                   {port, Config:get(server.port)}
               ]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Method = Req:get(method),
    Method1 = atom_to_list(Method),
    Method2 = string:to_lower(Method1),
    Method3 = list_to_atom(Method2),
    apply(webservice, Method3, [Req:get(path), Req]).

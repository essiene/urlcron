-module(urlcron_mochiweb).
-export([
        start/0, 
        start/1, 
        stop/0, 
        ping/0,
        loop/1
    ]).

%% External API

start() ->
    start(erlcfg:new()).

start(Config) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
           mochiweb_http:start([
                   {name, ?MODULE}, 
                   {loop, Loop}, 
                   {ip, Config:get(server.listen, "0.0.0.0")},
                   {port, Config:get(server.port, 8118)}
               ]).

stop() ->
    mochiweb_http:stop(?MODULE).

ping() ->
    case whereis(urlcron_mochiweb) of
        undefined ->
            pang;
        Pid when is_pid(Pid) ->
            pong
    end.


loop(Req) ->
    Method = Req:get(method),
    Method1 = atom_to_list(Method),
    Method2 = string:to_lower(Method1),
    Method3 = list_to_atom(Method2),
    apply(webservice, Method3, [Req:get(path), Req]).

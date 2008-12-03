-module(urlcronctl).
-export([
        action/0,
        action/1
    ]).


action([exit]) ->
    display(
        urlcron_exec(init, stop, [], exited)
    );

action([stop]) ->
    display(
        urlcron_exec(urlcron, stop, [], stopped)
    );

action([start]) ->
    display(
        urlcron_exec(urlcron, start, [], started)
    );

action([restart]) ->
    action([stop]),
    action([start]);

action([status]) ->
    display(
        urlcron_exec(urlcron, status, [])
    );

action(Any) ->
    available_commands(Any).

action() ->
    action([status]).

available_commands(_WrongCommand) ->
    OutString = "urlcronctl [actions]\n" 
    "actions:\n" 
    "   exit    - Takes down a urlcron node.\n"
    "   stop    - Stops the running urlcron instance, without\n"
    "             bringing down the node.\n"
    "   start   - Start urlcron on an alive node.\n"
    "   status  - Report the status of an alive node.\n"
    "   restart - Restart urlcron on an alive node.",
    io:format("~s\n", [OutString]).

display([]) ->
    do_nothing;
display({Key, Val}) ->
    display(Key),
    display(" - ["),
    display(Val),
    display("]");
display([{_Key, _Val}=Head | Rest]) ->
    display(Head),
    display("\n"),
    display(Rest);
display(Atom) when is_atom(Atom) ->
    io:format("~p", [Atom]);
display(String) when is_list(String) ->
    io:format("~s", [String]).

urlcron_exec(Module, Fun, Args) ->
    UrlcronNode = get_urlcron_node(),
    case rpc:call(UrlcronNode, Module, Fun, Args) of
        {badrpc, node_down} ->
            [{urlcron, dead}];
        {badrpc, _Reason} ->
            [{urlcron, stopped}];        
        Response ->
            Response
    end.

urlcron_exec(Module, Fun, Args, OkResponse) ->
    case urlcron_exec(Module, Fun, Args) of
        [{urlcron, _Reason}] = Response ->
            Response;
        _Other ->
            [{urlcron, OkResponse}]
    end.


get_urlcron_node() ->
    [UrlcronNode] = init:get_plain_arguments(),
    list_to_atom(UrlcronNode).

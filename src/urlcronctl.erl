-module(urlcronctl).
-export([
        action/0,
        action/1
    ]).


action([exit]) ->
    urlcron_exec(init, stop, []),
    display({urlcron, "Exited"});

action([stop]) ->
    urlcron_exec(urlcron, stop, []),
    display({urlcron, "Stopped"});

action([start]) ->
    urlcron_exec(urlcron, start, []),
    display({urlcron, "Started"});

action([status]) ->
    display(urlcron_exec(urlcron, status, []));

action(Any) ->
    available_commands(Any).

action() ->
    action([status]).

available_commands(_WrongCommand) ->
    OutString = " \
    ",
    io:format("~s\n", [OutString]).

display([]) ->
    do_nothing;
display({Key, Val}) ->
    ValStr = string:join(["[", Val, "]"], ""),
    KeyValStr = string:join([atom_to_list(Key), ValStr], " - "),
    display(KeyValStr);
display([{_Key, _Val}=Head | Rest]) ->
    display(Head),
    display(Rest);
display(String) ->
    io:format("~s\n", [String]).

urlcron_exec(Module, Fun, Args) ->
    UrlcronNode = get_urlcron_node(),
    case rpc:call(UrlcronNode, Module, Fun, Args) of
        {badrpc, nodedown} ->
            {urlcron, "Dead"};
        Response ->
            Response
    end.


get_urlcron_node() ->
    [UrlcronNode] = init:get_plain_arguments(),
    list_to_atom(UrlcronNode).

-module(webservice).
-export([
       get/2
    ]).


get("/", Req) ->
    get("/view", Req);
get("/view/", Req) ->
    get("/view", Req);
get("/view", Req) ->
    [H, N, L] = pqueueserv:get_all(),
    H1 = callentry_util:strjoin(H, "\n"),
    N1 = callentry_util:strjoin(N, "\n"),
    L1 = callentry_util:strjoin(L, "\n"),

    Format = "Queue Total: ~p~nQueue High/Medium/Low: ~p~nQueued:~n-~n~s~n-~n~s~n-~n~s~n",
    Req:ok({"text/plain", io_lib:format(Format, [pqueueserv:get_total(), pqueueserv:get_size(), H1, N1, L1])});
get("/add", Req) ->
    try
        {CallEntry, Priority} = from_request(Req),
        pqueueserv:enqueue(CallEntry, Priority),
        Req:ok({"text/plain", io_lib:format("ENQ.0: ~s - ~s", [CallEntry:to_string(), format_priority(Priority)])})
    catch
        throw: {Code, Error, Detail} ->
%            util:logmessage({Code, Error, Detail}),
            Req:ok({"text/plain", io_lib:format("ENQ.~p: ~p - ~s", [Code, Error, Detail])})
    end;
get(_Path, Req) ->
    Req:not_found().





from_request(Req) ->
    Params = Req:parse_qs(),

    CallId = get_param(Params, "callid"),
    MsisdnList = string:tokens(get_param(Params, "msisdn"), " "),
    Extra = get_param(Params, "extra", ""),
    Rttl = get_param_int(Params, "rttl", 3),
    Qttl = get_param_int(Params, "qttl", 60) * 1000,

    Priority = get_param_atom(Params, "priority", normal),

    CallEntry = callentry_util:create(CallId, MsisdnList, Extra, Rttl, Qttl),

    {CallEntry, Priority}.


get_param(Proplist, Key) ->
    case proplists:lookup(Key, Proplist) of
        none ->
            throw({501, not_set, Key});
        {Key, []} ->
            throw({502, empty, Key});
        {Key, Value} ->
            Value
    end.

get_param(Proplist, Key, Default) ->
    try
        get_param(Proplist, Key) 
    catch
        throw: {_Code, _Error, _Key} ->
            Default
    end.

%get_param_atom(Params, Key) ->
%    list_to_atom(string:to_lower(get_param(Params, Key))).

get_param_atom(Params, Key, Default) ->
    case get_param(Params, Key, Default) of
        Value when is_atom(Value) ->
            Value;
        Value when is_list(Value) ->
            list_to_atom(string:to_lower(Value))
    end.

%get_param_int(Params, Key) ->
%    list_to_integer(get_param(Params, Key)).

get_param_int(Params, Key, Default) ->
    case get_param(Params, Key, Default) of
        Value when is_integer(Value) ->
            Value;
        Value when is_list(Value) ->
            list_to_integer(Value)
    end.


format_priority(Priority) ->
    String = atom_to_list(Priority),
    string:to_upper(String).

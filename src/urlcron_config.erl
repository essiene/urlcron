-module(urlcron_config).
-export([
        new/0,
        get/2
    ]).



new() -> 
    [ 
        {listen, get_param_str(listen, "0.0.0.0")},
        {port, get_param(port, 8118)},
        {log_home, get_param_str(log_home, "/var/log/urlcron")},
        {data_home, get_param_str(data_home, "/var/lib/urlcron")}
    ].

get(Config, Key) ->
    case lists:keysearch(Key, 1, Config) of
        false ->
            throw({508, unknown_config_property, Key});
        {value, {Key, Value}} ->
            Value
    end.



get_param(Key) ->
    case application:get_env(Key) of
        undefined ->
            throw({507, config_undefined, Key});
        {ok, Value} ->
            Value
    end.

get_param(Key, Default) ->
    try
        get_param(Key)
    catch
        throw: {507, config_undefined, Key} ->
            Default
    end.

get_param_str(Key) ->
    Param = get_param(Key),
    to_string(Param).

get_param_str(Key, Default) ->
    try
        get_param_str(Key)
    catch
        throw: {507, config_undefined, Key} ->
            Default
    end.

to_string(Any) when is_atom(Any) ->
    atom_to_list(Any);
to_string(Any) when is_integer(Any) ->
    integer_to_list(Any).

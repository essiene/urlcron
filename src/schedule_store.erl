-module(schedule_store).
-export([
        init/1
    ]).
-include("urlcron.hrl").



init(Config) ->
    create_mnesia_schema(Config),
    create_mnesia_tables().

create_mnesia_schema(Config) ->
    mnesia:stop(),

    MnesiaDir = Config:get(server.home.data, "/tmp/"),
    application:set_env(mnesia, dir, MnesiaDir),

    Node = node(),
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        Other ->
            throw(Other)
    end.

create_mnesia_tables() ->
    mnesia:start(),

    TableDef = [
        {attributes, record_info(fields, schedule)},
        {disc_copies, [node()]},
        {type, set}
    ],

    case mnesia:create_table(schedule, TableDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, schedule}} ->
            ok;
        {aborted, Reason} ->
            throw(Reason)
    end.

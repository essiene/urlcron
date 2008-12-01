-module(schedule_store).
-export([
        add/5,
        get/1,
        update/1,
        update/2
    ]).

-export([
        start/0,
        start/1,
        stop/0,
        destroy/0,
        destroy/1
    ]).

-include("urlcron.hrl").


start() ->
    start(erlcfg:new()).

start(Config) ->
    mnesia:stop(),
    create_mnesia_schema(Config),

    mnesia:start(),
    create_mnesia_tables(Config).

stop() ->
    mnesia:stop().

destroy() ->
    destroy(erlcfg:new()).

destroy(Config) ->
    mnesia:start(),
    delete_mnesia_tables(),
    mnesia:stop(),
    delete_mnesia_schema(Config).


add(Name, Pid, StartTime, Url, Status) ->
    Schedule = #schedule{name=Name, pid=Pid, start_time=StartTime, url=Url, status=Status},
    save(Schedule).



get(Name) ->
    Fun = fun() ->
        case mnesia:read({schedule, Name}) of
            [Schedule] -> 
                Schedule;
            [] ->
                {error, not_found}
        end
    end,
    transaction(Fun).



update(Schedule, []) ->
    update(Schedule);

update(Schedule, [{start_time, Value} | Rest]) when is_record(Schedule, schedule) ->
    NewSchedule = Schedule#schedule{start_time = Value},
    update(NewSchedule, Rest);

update(Schedule, [{url, Value} | Rest]) when is_record(Schedule, schedule) ->
    NewSchedule = Schedule#schedule{url = Value},
    update(NewSchedule, Rest);

update(Name, ValueList) ->
    case schedule_store:get(Name) of
        {error, not_found} ->
            {error, not_found};
        #schedule{status=completed} ->
            {error, schedule_already_completed};
        Schedule ->
            update(Schedule, ValueList)
    end.

update(Schedule) ->
    save(Schedule).

save(Schedule) when is_record(Schedule, schedule) ->
    Fun= fun() ->
            mnesia:write(Schedule)
    end,
    transaction(Fun).

transaction(Trans) ->
    case mnesia:transaction(Trans) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.



create_mnesia_schema(Config) ->
    MnesiaDir = Config:get(server.home.data, "/tmp/"),
    application:set_env(mnesia, dir, MnesiaDir),

    Node = list_to_atom(Config:ensure_get(cluster.master)),
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        Other ->
            throw(Other)
    end.

delete_mnesia_schema(Config) ->
    MnesiaDir = Config:get(server.home.data, "/tmp"),
    application:set_env(mnesia, dir, MnesiaDir),

    Node = list_to_atom(Config:ensure_get(cluster.master)),
    case mnesia:delete_schema([Node]) of
        ok ->
            ok;
        {error, Reason} ->
            throw(Reason)
    end.





create_mnesia_tables(Config) ->
    MasterNode = list_to_atom(Config:ensure_get(cluster.master)),
    SlaveNodes = [list_to_atom(Node) || Node <- Config:ensure_get(cluster.slaves)],
    TableDef = [
        {attributes, record_info(fields, schedule)},
        {disc_copies, [MasterNode]},
        {ram_copies, SlaveNodes},
        {type, set}
    ],

    case mnesia:create_table(schedule, TableDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, schedule}} ->
            ok;
        {aborted, Reason1} ->
            throw(Reason1)
    end.

delete_mnesia_tables() ->
    mnesia:delete_table(schedule).

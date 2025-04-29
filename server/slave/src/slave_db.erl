-module(slave_db).
-export([create_tables/1, get_status/0, update_status_field/2]).

-record(status, {key, value}).

create_tables(Node) ->
    _ = case mnesia:create_schema([Node]) of
            ok                        -> io:format("[INFO] Schema creato~n", []);
            {error, {_, {already_exists,_}}}   -> io:format("[INFO] Schema già esistente~n", [])
        end,

    ok = mnesia:start(),

    Result = mnesia:create_table(status, [
        {attributes, record_info(fields, status)},  %% => [key,value]
        {ram_copies, [Node]},
        {type, set}
    ]),
    io:format("[DEBUG] mnesia:create_table(status): ~p~n", [Result]),

    case mnesia:wait_for_tables([status], 500) of
        ok ->
            io:format("[INFO] Tabella 'status' pronta~n", []);
        {timeout, Missing} ->
            io:format("[ERROR] wait_for_tables timeout: mancanti ~p~n", [Missing]),
            exit({wait_timeout, Missing})
    end,

    initialize_status().

initialize_status() ->
    F = mnesia:transaction(fun() ->
        mnesia:write(#status{key = possible_requests, value = 0}),
        mnesia:write(#status{key = pending_requests, value = 0})
    end),
    case F of
        {atomic, ok} -> 
            io:format("[INFO] Status initialized successfully~n"),
            {ok, initialized};
        {aborted, Reason} -> 
            io:format("[INFO] Status not initialized ~p ~n", [Reason]),
            {error, Reason}
    end.

get_status() ->
    F = mnesia:transaction(fun() ->
        mnesia:match_object(#status{key = '_', value = '_'})
    end),
    io:format("[INFO] Status: ~p~n", [F]),
    case F of
        {atomic, []} ->
            {error, not_found};
        {atomic, Records} ->
            Map = lists:foldl(fun(#status{key = Key, value = Value}, Acc) ->
                                    Acc#{Key => Value}
                                end, #{}, Records),
            {ok, Map};
        {aborted, Reason} ->
            {error, Reason}
    end.

update_status_field(Field, Increment) ->
    io:format("[INFO] Update status field: ~p~n", [Field]),
    io:format("[INFO] Increment: ~p~n", [Increment]),
    F = mnesia:dirty_update_counter(status, Field, Increment),
    io:format("[INFO] Update status field: ~p~n", [F]).

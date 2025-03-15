-module(master_db).
-export([create_tables/1,insert_file/2, get_file/1,  insert_chunk/4, get_chunks/2]).

-record(user, {user_file, num_chuncks}).
-record(chunk, {id, chunk_name, nodes}).

create_tables(Nodes) when is_list(Nodes) ->
    mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, Nodes},
        {type, bag},
        {record_name, user}
    ]),
    
    mnesia:create_table(chunk, [
        {attributes, record_info(fields, chunk)},
        {disc_copies, Nodes},
        {type, set},
        {record_name, chunk}
    ]).



insert_file(FileName, NumChunks) ->
    F = mnesia:transaction(fun() ->
        mnesia:write(#user{user_file = {<<"test">>, FileName}, num_chuncks = NumChunks})
    end),
    case F of
        {atomic, ok} ->             
            io:format("file inserito~n"),
            {ok, inserted};
        {aborted, Reason} -> 
            io:format("file inserito ~p ~n", [Reason]),
            {error, Reason}
    end.

get_file(FileName) ->
    F = mnesia:transaction(fun() ->
        mnesia:read({user, {<<"test">>, FileName}})
    end),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, [Record]} -> {ok, Record};
        {aborted, Reason} -> {error, Reason}
    end.

insert_chunk(FileName, ChunkName, Position, Nodes) ->
    F = mnesia:transaction(fun() ->
        mnesia:write(#chunk{id = {FileName, Position},
                                chunk_name = ChunkName,
                                nodes = Nodes})
    end),
    case F of
        {atomic, ok} -> 
            io:format("inseritooooo~n"),
            {ok, inserted};
        {aborted, Reason} -> 
            io:format("errore ~p~n",[Reason]),
            {error, Reason}
    end.

get_chunks(FileName, Chuncks) ->
    get_chunks(FileName, Chuncks - 1, []).

get_chunks(FileName, Chuncks, Acc) when Chuncks >= 0 ->
    F = mnesia:transaction(fun() ->
        mnesia:read({chunk, {FileName, Chuncks}})
    end),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, Records} -> get_chunks(FileName, Chuncks - 1, [Records] ++ Acc);
        {aborted, Reason} -> {error, Reason}
    end;
get_chunks(_, Chuncks, Acc) when Chuncks < 0 ->
    Acc.
    


    
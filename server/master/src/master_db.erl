-module(master_db).
-export([create_tables/1, insert_file/3, get_file/1, insert_chunk/4, get_chunks/2, insert_user/2, get_user/1]).

-record(user_file, {user_file, num_chuncks}).
-record(chunk, {id, chunk_name, nodes}).
-record(user, {username, password}).

create_tables(Nodes) when is_list(Nodes) ->
    mnesia:create_table(user_file, [
        {attributes, record_info(fields, user_file)},
        {disc_copies, Nodes},
        {type, bag},
        {record_name, user_file}
    ]),
    
    mnesia:create_table(chunk, [
        {attributes, record_info(fields, chunk)},
        {disc_copies, Nodes},
        {type, set},
        {record_name, chunk}
    ]),

    mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, Nodes},
        {type, set},
        {record_name, user}
    ]).


insert_user(Username, Password) ->
    HashedPassword = crypto:hash(sha256, Password),
    F = mnesia:transaction(fun() ->
        mnesia:write(#user{username = Username, password = HashedPassword})
    end),
    case F of
        {atomic, ok} -> 
            io:format("[INFO] User successfully inserted~n"),
            {ok, inserted};
        {aborted, Reason} -> 
            io:format("[INFO] User not inserted ~p ~n", [Reason]),
            {error, Reason}
    end.

get_user(Username) ->
    F = mnesia:transaction(fun() ->
        mnesia:read({user, Username})
    end),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, [Record]} -> {ok, Record};
        {error, not_exists} -> {error, not_found};        
        {aborted, Reason} -> {error, Reason}
    end.

insert_file(Username, FileName, NumChunks) ->
    F = mnesia:transaction(fun() ->
        mnesia:write(#user_file{user_file = {<<"test">>, FileName}, num_chuncks = NumChunks})
    end),
    case F of
        {atomic, ok} ->             
            io:format("[INFO] File successfully inserted~n"),
            {ok, inserted};
        {aborted, Reason} -> 
            io:format("[INFO] File not inserted ~p ~n", [Reason]),
            {error, Reason}
    end.

get_file(FileName) ->
    F = mnesia:transaction(fun() ->
        mnesia:read({user_file, {<<"test">>, FileName}})
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
            io:format("[INFO] Chunk successfully inserted~n"),
            {ok, inserted};
        {aborted, Reason} -> 
            io:format("[INFO] Chunk not inserted ~p ~n", [Reason]),
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
    


    
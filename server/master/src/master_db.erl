-module(master_db).
-export([test/0, create_tables/1, insert_file/3, insert_file/4, 
    get_file/1, get_files/1, insert_chunk/4, get_chunks/2, 
    insert_user/2, get_user/1, count_chunks/1]).

-record(user_file, {user_file, file_id, num_chuncks}).
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

get_files(Username) ->
    io:format("Format: ~p~n", [Username]),
    F = mnesia:transaction(fun() ->
        mnesia:match_object(#user_file{
            user_file   = {Username, '_'},
            file_id = '_',
            num_chuncks = '_'
        })
    end),
    io:format("[INFO] Files: ~p~n", [F]),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, Records} -> {ok, Records};
        {aborted, Reason} -> {error, Reason}
    end.


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
    FileID = now_secs_hashed_hex(),
    F = mnesia:transaction(fun() ->
        mnesia:write(#user_file{user_file = {Username, FileName}, 
            file_id = FileID, num_chuncks = NumChunks})
    end),
    case F of
        {atomic, ok} ->
            io:format("[INFO] File successfully inserted~n"),
            {ok, FileID};
        {aborted, Reason} -> 
            io:format("[INFO] File not inserted ~p ~n", [Reason]),
            {error, Reason}
    end.

insert_file(Username, FileName, FileID, NumChunks) ->

    F = mnesia:transaction(fun() ->
        mnesia:write(#user_file{user_file = {Username, FileName}, file_id = FileID, num_chuncks = NumChunks})
    end),
    case F of
        {atomic, ok} ->
            io:format("[INFO] File successfully inserted ~p ~n",[#user_file{user_file = {Username, FileName}, file_id = FileID, num_chuncks = NumChunks}]),
            {ok, FileID};
        {aborted, Reason} -> 
            io:format("[INFO] File not inserted ~p ~n", [Reason]),
            {error, Reason}
    end.
    

get_file(FileID) ->
    F = mnesia:transaction(fun() ->
        mnesia:match_object(#user_file{
            user_file   = {'_', '_'},
            file_id = FileID,
            num_chuncks = '_'
        })
    end),
    io:format("[INFO] Files: ~p~n", [F]),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, [Records]} -> {ok, Records};
        {aborted, Reason} -> {error, Reason}
    end.

insert_chunk(FileID, ChunkName, Position, Nodes) ->
    F = mnesia:transaction(fun() ->
        mnesia:write(#chunk{id = {FileID, Position},
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

get_chunks(FileID, Chuncks) ->
    get_chunks(FileID, Chuncks - 1, []).

get_chunks(FileID, Chuncks, Acc) when Chuncks >= 0 ->
    F = mnesia:transaction(fun() ->
        mnesia:read({chunk, {FileID, Chuncks}})
    end),
    case F of
        {atomic, []} -> {error, not_found};
        {atomic, Records} -> get_chunks(FileID, Chuncks - 1, [Records] ++ Acc);
        {aborted, Reason} -> {error, Reason}
    end;
get_chunks(_, Chuncks, Acc) when Chuncks < 0 ->
    Acc.
    
count_chunks(Node) ->
    % accumulator function, for each chunk record, 
    % if the node is in the list of nodes, increment the accumulator
    F = fun(#chunk{nodes = Nodes}, Acc) ->
                case lists:member(Node, Nodes) of
                    true -> Acc + 1;
                    false -> Acc
                end
            end,
    % use a mnesia transaction to fold over the chunk table
    {atomic, Count} = mnesia:transaction(fun() ->
            mnesia:foldl(F, 0, chunk)
    end),
    Count.
    
now_secs_hashed_hex() ->
    TS = os:system_time(second),
    HashBin = crypto:hash(sha256, integer_to_binary(TS)),
    HexStr = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(HashBin)]),
    list_to_binary(HexStr).
    
test() -> 
    mnesia:activity(transaction, fun() ->
    mnesia:match_object(#user_file{file_id='_', user_file='_', num_chuncks='_'})
    end).

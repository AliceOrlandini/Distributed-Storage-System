-module(master_db).
-export([insert_file/3, insert_file/4, 
    get_file/2, get_files/1, insert_chunk/4, get_chunks/2, 
    insert_user/2, get_user/1, count_chunks/1, has_other_owners/2, 
    remove_chunks/2, delete_file/2, init_db_bootstrap/1, check_and_sync_db/0]).

-record(user_file, {user_file, file_id, num_chuncks}).
-record(chunk, {id, chunk_name, nodes}).
-record(user, {username, password}).

init_db_bootstrap(Nodes) ->
    io:format("[DEBUG] Nodes ~p~n", [application:get_env('Distributed-Storage-System', replica_nodes)]),
    lists:foreach(fun(N) -> 
        io:format("[DEBUG] connected: ~p~n",[net_kernel:connect_node(N)])
    end, Nodes),

    io:format("create schema ~p~n", [mnesia:create_schema([node() | Nodes])]),

    lists:foreach(fun(N) ->
        rpc:call(N, application, start, [mnesia])
    end, [node() | Nodes]),

    {ok, Res} = application:ensure_all_started(mnesia),
    io:format("[DEBUG] Mnesia started: ~p~n", [Res]),
    
    create_tables(Nodes).


create_tables(Nodes) ->
   
    mnesia:create_table(chunk, [
        {attributes, record_info(fields, chunk)},
        {disc_copies, [node() | Nodes]},
        {type, set},
        {record_name, chunk}
    ]),

    mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node() | Nodes]},
        {type, set},
        {record_name, user}
    ]),

    mnesia:create_table(user_file, [
        {attributes, record_info(fields, user_file)},
        {disc_copies, [node() | Nodes]},
        {type, set},
        {record_name, user_file}
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

delete_file(Username, FileID) ->
    case mnesia:transaction(fun() ->
                mnesia:match_object(#user_file{
                user_file   = {Username, '_'},
                file_id     = FileID,
                num_chuncks = '_'
                })
            end) of
        {atomic, []} ->
            {error, not_found};
        {atomic, Records} ->
            _ = mnesia:transaction(fun() ->
                    lists:foreach(fun(Rec) ->
                        mnesia:delete_object(Rec)
                    end, Records)
                end),
            io:format("[INFO] File successfully deleted~n"),
            {ok, deleted};
        {aborted, Reason} ->
            io:format("[INFO] File not deleted ~p~n", [Reason]),
            {error, Reason}
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
            io:format("[INFO] File successfully inserted ~p ~n", 
                [#user_file{user_file = {Username, FileName}, file_id = FileID, num_chuncks = NumChunks}]),
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
    

get_file(Username, FileID) ->
    F = mnesia:transaction(fun() ->
        mnesia:match_object(#user_file{
            user_file   = {Username, '_'},
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

has_other_owners(FileID, Username) ->
    case mnesia:transaction(fun() ->
        mnesia:match_object(#user_file{user_file = {'_', '_'}, file_id = FileID, num_chuncks = '_'})
    end) of
        {atomic, Records} ->
            lists:any(fun(#user_file{user_file = {U, _}}) -> U =/= Username end, Records);
        _ -> false
    end.

remove_chunks(FileID, NumChunks) ->
    lists:foreach(fun(N) ->
        mnesia:transaction(fun() ->
            mnesia:delete({chunk, {FileID, N}})
        end)
    end, lists:seq(0, NumChunks-1)).

check_and_sync_db() ->
    case schema_exists() of
        true -> 
            io:format("[INFO] Schema exists~n"),
            mnesia:start(),
            ok;
        false -> 
            io:format("[INFO] Schema not exists~n"),
            {error, timeout}
    end.


schema_exists() ->
    Nodes = mnesia:system_info(db_nodes),
    NodesStr = case application:get_env('Distributed-Storage-System', replica_nodes) of
        {ok, N} when is_list(N) -> N;
        _ -> [node()]
        end,
    ReplicaNodes = [case N of
                S when is_list(S) -> list_to_atom(S);
                A when is_atom(A) -> A
             end || N <- NodesStr],

    lists:all(fun(N) -> lists:member(N, Nodes) end, [node() | ReplicaNodes]).


-module(master_upload_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [#{username := Username}] = Opts,
    io:format("[INFO] Username: ~p~n", [Username]),
    Req2 = handle_request(Method, Req, Username),
    {ok, Req2, Opts}.

handle_request(<<"POST">>, Req, Username) ->
    process_upload(Req, Username);
handle_request(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

process_upload(Req, Username) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Req, #{}),
    {ok, Data, _} = cowboy_req:read_part_body(Req2),
    
    {file, Filename, _ContentType, _BitSize} = cow_multipart:form_data(Headers),
    
    Body = [{<<"filename">>, Filename},
            {<<"base64_file">>, base64:encode(Data)}],
    
    Chunks = file_chunks:devide_into_chunks(Data, 1024),
    
    {ok, FileID} = master_db:insert_file(Username, Filename, length(Chunks)),
    
    send_chunks_to_nodes(Chunks, FileID, 0), 
    {ok, Body, Req}.

send_chunks_to_nodes([Chunk | Rest], FileID, Position) ->
    Nodes = get_slave_nodes(),
    
    Hash = hash_chunck:get_hash(Chunk),
    MainNode = get_least_loaded_node(Nodes, undefined),
    io:format("[INFO] Selected main slave node: ~p~n", [MainNode]),
    ReplicationNode = get_least_loaded_node(Nodes, MainNode),
    io:format("[INFO] Selected replication slave node: ~p~n", [ReplicationNode]),
    {slave, MainNode} ! {file, Hash, Chunk, ReplicationNode},
    master_db:insert_chunk(FileID, Hash, Position, [MainNode, ReplicationNode]),    
    send_chunks_to_nodes(Rest, FileID, Position + 1);
send_chunks_to_nodes([], _FileID, _Position) ->
    ok.

get_slave_nodes() ->
    case application:get_env('Distributed-Storage-System', slave_nodes) of
        {ok, NodesStr} when is_list(NodesStr) ->
            lists:map(fun(NodeStr) -> list_to_atom(NodeStr) end, NodesStr);
        _ ->
            []
    end.

get_least_loaded_node(Nodes, ExcludedNode) ->
    FilteredNodes = lists:filter(fun(Node) -> Node =/= ExcludedNode end, Nodes),
    LoadedNodes = [{Node, master_db:count_chunks(Node)} || Node <- FilteredNodes],
    io:format("[INFO] Loaded nodes: ~p~n", [LoadedNodes]),
    case LoadedNodes of
        [] ->
            error(no_nodes_available);
        _ ->
            [{LeastLoadedNode, _} | _] = lists:keysort(2, LoadedNodes),
            LeastLoadedNode
    end.

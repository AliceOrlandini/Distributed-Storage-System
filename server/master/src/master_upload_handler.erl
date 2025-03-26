-module(master_upload_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [#{username := Username}] = Opts,
    io:format("[INFO] Username: ~p~n", [Username]),
    Reply = handle(Method, Req, Username),
    {ok, Reply, Opts}.

handle(<<"POST">>,  Req, Username) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Req, #{}),
    {ok, Data, _} = cowboy_req:read_part_body(Req2),

    {file, Filename, _ContentType, _BitSize} = cow_multipart:form_data(Headers),

    Body = [{<<"filename">>, Filename}, {<<"base64_file">>, base64:encode(Data)}],
    Chunks = file_chunks:devide_into_chunks(Data, 80*1024),
    io:format("[INFO] FileName: ~p~n", [Filename]),
    {ok, FileID} = master_db:insert_file(Username, Filename, length(Chunks)),
    send_chunks_to_node(Chunks, FileID, 0),
    {ok, Body, Req};
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).


send_chunks_to_node([Head|Tail], FileID, Position) ->
    Nodes = get_slave_nodes(),

    Hash = hash_chunck:get_hash(Head),

    % select the main node as the one with the least number of chunks
    MainNode = get_least_loaded_node(Nodes, undefined),
    io:format("[INFO] Selected main slave node: ~p~n", [MainNode]),

    % select the replication node excluding the main node
    ReplicationNode = get_least_loaded_node(Nodes, MainNode),
    io:format("[INFO] Selected replication slave node: ~p~n", [ReplicationNode]),

    % send the chunk to the main node
    {slave, MainNode} ! {file, Hash, Head, ReplicationNode},

    % insert the chunk in the database
    master_db:insert_chunk(FileID, Hash, Position, [MainNode, ReplicationNode]),

    % process the next chunk
    send_chunks_to_node(Tail, FileID, Position + 1);
send_chunks_to_node([], _, _) ->
    ok.

get_slave_nodes() ->
    case application:get_env('Distributed-Storage-System', slave_nodes) of
        {ok, NodesStr} ->
            lists:map(fun(NodeStr) -> list_to_atom(NodeStr) end, NodesStr);
        undefined ->
            []
    end.

get_least_loaded_node(Nodes, ExcludedNode) ->
    % filter out the excluded node
    FilteredNodes = lists:filter(fun(Node) -> Node =/= ExcludedNode end, Nodes),
    % create a list of tuples with the node and the number of chunks
    LoadedNodes = [{Node, master_db:count_chunks(Node)} || Node <- FilteredNodes],
    io:format("[INFO] Loaded nodes: ~p~n", [LoadedNodes]),
    % if there are no nodes available, return an error
    case LoadedNodes of
        [] ->
            error(no_nodes_available);
        _ ->
            % select the node with the least number of chunks
            [{LeastLoadedNode, _}|_] = lists:keysort(2, LoadedNodes),
            LeastLoadedNode
    end.
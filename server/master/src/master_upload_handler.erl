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

    master_db:insert_file(Username, Filename, length(Chunks)),
    send_chunks_to_node(Chunks, Username, Filename, 0),
    {ok, Body, Req};
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).


send_chunks_to_node([Head|Tail], Username, FileName, Position) ->
    Nodes = get_slave_nodes(),
    LenNodes = length(Nodes),
    Hash = hash_chunck:get_hash(Head),
    Integer = binary:decode_unsigned(Hash),
    NodeToSave = Integer rem LenNodes,
    NodeDest = lists:nth(NodeToSave + 1, Nodes),
    {slave, NodeDest} ! {file, Hash, Head},
    master_db:insert_chunk(Username, FileName, Hash, Position, [NodeDest]),
    send_chunks_to_node(Tail, Username, FileName, Position+1);
send_chunks_to_node([],_,_,_) ->
    ok.

get_slave_nodes() ->
    case application:get_env('Distributed-Storage-System', slave_nodes) of
        {ok, NodesStr} ->
            lists:map(fun(NodeStr) -> list_to_atom(NodeStr) end, NodesStr);
        undefined ->
            []
    end.

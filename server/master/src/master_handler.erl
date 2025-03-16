-module(master_handler).
-behaviour(cowboy_handler).
-behaviour(application).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req),
    {ok, Reply, Opts}.

handle(<<"POST">>,  Req) ->
    {ok, Headers, Req2} = cowboy_req:read_body(Req, #{length => infinit}),
    {ok, Data, _Req3} = cowboy_req:read_part_body(Req2),
    {file, Filename, _ContentType, _BitSize} = cow_multipart:form_data(Headers),
    Body = [{<<"filename">>, Filename}, {<<"base64_file">>, base64:encode(Data)}],
    Chunks = file_chunks:devide_into_chunks(Data, 8),
    master_db:insert_file(Filename, length(Chunks)),
    send_chunks_to_node(Chunks,Filename, 0),
    {ok, Body, Req};
handle(_, Req) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).


send_chunks_to_node([Head|Tail],FileName, Position) ->
    Nodes = get_slave_nodes(),
    LenNodes = length(Nodes),
    Hash = hash_chunck:get_hash(Head),
    Integer = binary:decode_unsigned(Hash),
    NodeToSave = Integer rem LenNodes,
    NodeDest = lists:nth(NodeToSave + 1, Nodes),
    {slave, NodeDest} ! {file, Hash, Head},
    master_db:insert_chunk(FileName, Hash, Position, [NodeDest]),
    send_chunks_to_node(Tail,FileName, Position+1);
send_chunks_to_node([],_,_) ->
    ok.

get_slave_nodes() ->
    case application:get_env(ws, slave_nodes) of
        {ok, NodesStr} ->
            lists:map(fun(NodeStr) -> list_to_atom(NodeStr) end, NodesStr);
        undefined ->
            []
    end.

-module(master_file_parts_handler).
-export([init/2]).
-behaviour(cowboy_handler).

init(Req, Opts) ->
    SecretKey = extract_option(secret_key, Opts),
    Username  = extract_option(username, Opts),
    Method = cowboy_req:method(Req),
    Req2 = handle_request(Method, Req, Username, SecretKey),
    {ok, Req2, Opts}.

extract_option(Key, Opts) ->
    case lists:filter(fun(Map) -> maps:is_key(Key, Map) end, Opts) of
        [Map | _] -> maps:get(Key, Map);
        [] -> undefined
    end.

handle_request(<<"GET">>, Req, Username, SecretKey) ->
    Qs0 = cowboy_req:parse_qs(Req),
    Qs = case is_list(Qs0) of
             true -> Qs0;
             false -> [Qs0]
         end,
    handle_get(Qs, Req, Username, SecretKey);
handle_request(_, Req, _, _) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

handle_get(Qs, Req, Username, SecretKey) ->
    case proplists:get_value(<<"fileID">>, Qs, undefined) of
        undefined ->
            cowboy_req:reply(400, #{}, <<"Missing fileID parameter">>, Req);
        FileID ->
            io:format("[INFO] FileID: ~p~n", [FileID]),
            process_file_parts(Username, FileID, Req, SecretKey)
    end.
    
process_file_parts(Username, FileID, Req, SecretKey) ->
    case master_db:get_file(Username, FileID) of
        {ok, FileRecord} ->
            process_file_record(FileRecord, FileID, Req, SecretKey);
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req)
    end.

process_file_record({user_file, {_Username, _Filename}, _FileID, NumChunks}, FileID, Req, SecretKey) ->
    JsonResponse = get_chunks_as_json(FileID, NumChunks, SecretKey),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Req).

get_chunks_as_json(FileID, NumChunks, SecretKey) ->
    Records = master_db:get_chunks(FileID, NumChunks),
    io:format("[INFO] Records: ~p~n", [Records]),
    ChunkList = map_chunks(Records, SecretKey),
    jiffy:encode(ChunkList).

map_chunks(Records, SecretKey) ->
    lists:reverse(map_chunks(Records, [], SecretKey)).

map_chunks([[{chunk, {_Filename, ChunkPosition}, ChunkName, Nodes}] | Tail], Acc, SecretKey) ->
    TokenChunkName = master_jwt:encode_file_name(ChunkName, SecretKey),
    ChosenNode = choose_node(Nodes),
    update_slaves(Nodes, ChosenNode),
    IpFormatted = node_to_url(ChosenNode),
    IpFormattedBin = list_to_binary(IpFormatted),
    ChunkMap = #{<<"ip">> => IpFormattedBin,
                 <<"chunkName">> => TokenChunkName,
                 <<"chunkPosition">> => ChunkPosition},
    map_chunks(Tail, [ChunkMap | Acc], SecretKey);
map_chunks([], Acc, _) ->
    Acc.

node_to_url(Node) when is_atom(Node) ->
    node_to_url(atom_to_list(Node));
node_to_url(NodeStr) when is_list(NodeStr) ->
    case string:split(NodeStr, "@") of
        [NodeName, Host] ->
            case string:substr(NodeName, 6, length(NodeName) - 5) of
                NumStr when is_list(NumStr) ->
                    Port = 5000 + list_to_integer(NumStr),
                    lists:flatten(io_lib:format("http://~s:~p", [Host, Port]));
                _ ->
                    "http://" ++ NodeStr
            end;
        _ ->
            "http://" ++ NodeStr
    end.

choose_node(Nodes) when is_list(Nodes) ->
    choose_node(Nodes, []).

choose_node([Head | Tail], Acc) ->
    {slave, Head} ! {status, node(), self()},
    receive
        {status, Pending, Possible} ->
            Value = Pending + Possible * 0.5,
            io:format("[INFO] Node: ~p, Pending: ~p, Possible: ~p, Value: ~p~n", 
                      [Head, Pending, Possible, Value]),
            choose_node(Tail, [{Head, Value} | Acc])
    end;
choose_node([], Acc) ->
    Sorted = lists:sort(fun({_, V1}, {_, V2}) -> V1 < V2 end, Acc),
    io:format("[INFO] Sorted nodes: ~p~n", [Sorted]),
    case Sorted of
        [{Node, _}|_] -> Node;
        [] -> undefined
    end.

update_slaves([Head | Tail], Chosen) when Head =:= Chosen ->
    {slave, Head} ! {choose},
    io:format("[INFO] Chosen node: ~p~n", [Head]),
    update_slaves(Tail, Chosen);
update_slaves([Head | Tail], Chosen) when Head =/= Chosen ->
    io:format("[INFO] Not chosen node: ~p~n", [Head]),
    {slave, Head} ! {not_choose},
    update_slaves(Tail, Chosen);
update_slaves([], _) ->
    ok.

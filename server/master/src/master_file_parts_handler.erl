-module(master_file_parts_handler).
-export([init/2]).

-behaviour(cowboy_handler).

init(Req, Opts) ->
    SecretKey = case lists:search(fun(M1) -> maps:is_key(secret_key, M1) end, Opts) of
        {value, M1} -> maps:get(secret_key, M1);
        false -> undefined
    end,
    Username = case lists:search(fun(M2) -> maps:is_key(username, M2) end, Opts) of
        {value, M2} -> maps:get(username, M2);
        false -> undefined
    end,
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req, Username, SecretKey),
    {ok, Req2, Opts}.


handle(<<"GET">>, Req, Username, SecretKey) ->
    [Qs] = cowboy_req:parse_qs(Req),
    io:format("[INFO] Query string: ~p~n", [Qs]),
    {<<"fileID">>, FileID} = Qs,
    io:format("[INFO] File string: ~p~n", [FileID]),

   
    case master_db:get_file(Username, FileID) of
        {ok, Chuncks} ->
            io:format("[INFO] Chuncks: ~p~n", [Chuncks]),
            {user_file, {_, _},_, NumChunks} = Chuncks,
            Json = get_chunks_as_json( FileID, NumChunks, SecretKey),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req)
    end;
handle(_, Req, _, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).

get_chunks_as_json(FileName, Chuncks, SecretKey) ->
    %% Recupera i record dei chunk dalla query
    Records = master_db:get_chunks(FileName, Chuncks),
    io:format("[INFO] Records: ~p~n", [Records]),
    %% Trasforma ogni record in una mappa
    Files = chunk_to_map(Records, [], SecretKey),
    jiffy:encode(Files).


chunk_to_map([[{chunk, {_Filename, ChunkPosition}, ChunkName, Nodes}]|Tail], Acc, SecretKey) ->
    TokenChunkName = jwt:encode_file_name(ChunkName, SecretKey),
    NodeChoosen = choose_node(Nodes),
    update_slaves(Nodes, NodeChoosen),
    IpFormatted = node_to_url(NodeChoosen),
    io:format("[INFO] IP: ~p~n", [IpFormatted]),
    IpFormattedBin = list_to_binary(IpFormatted),
    Map = #{<<"ip">> => IpFormattedBin,
            <<"chunkName">> => TokenChunkName,  
            <<"chunkPosition">> => ChunkPosition},
    chunk_to_map(Tail, [Map | Acc], SecretKey);
chunk_to_map([], Acc, _) ->
    lists:reverse(Acc).


node_to_url(NodeStr) when is_atom(NodeStr) ->
    node_to_url(atom_to_list(NodeStr));
node_to_url(NodeStr) when is_list(NodeStr) ->
    case string:split(NodeStr, "@") of
        [NodeName, Host] ->
            % Estrai il numero dal nome del nodo (es. "slave3" -> 3)
            case string:substr(NodeName, 6, length(NodeName) - 5) of
                NumStr when is_list(NumStr) ->
                    Port = 5000 + list_to_integer(NumStr),
                    lists:flatten(io_lib:format("http://~s:~p", [Host, Port]));
                _ ->
                    "http://" ++ NodeStr  % fallback
            end;
        _ ->
            "http://" ++ NodeStr  % fallback
    end.


choose_node(Nodes) when is_list(Nodes) ->
    choose_node(Nodes, []).

choose_node([Head|Tail], Acc) ->
    {slave, Head} ! {status, node(), self()},
    receive
        {status, Pending, Possible} ->
            Value = Pending + Possible * 0.5,
            io:format("[INFO] Node: ~p, Pending: ~p, Possible: ~p, Value: ~p~n", [Head, Pending, Possible, Value]),
            choose_node(Tail, [{Head, Value} | Acc])
    end;
choose_node([], Acc) ->
    % Sort the list of nodes by value
    Sorted = lists:sort(fun({_, V1}, {_, V2}) -> V1 < V2 end, Acc),
    io:format("[INFO] Sorted nodes: ~p~n", [Sorted]),
    % Get the node with the least value
    case Sorted of
        [{Node, _}|_] -> Node;
        [] -> undefined
    end.

update_slaves([Head | Tail], Choosen) when Head =:= Choosen ->
    {slave, Head} ! {choose},
    io:format("[INFO] Choosen node: ~p~n", [Head]),
    update_slaves(Tail, Choosen);
update_slaves([Head | Tail], Choosen) when Head =/= Choosen ->
    io:format("[INFO] Not choosen node: ~p~n", [Head]),
    {slave, Head} ! {not_choose},
    update_slaves(Tail, Choosen);
update_slaves([], _) ->
    ok.
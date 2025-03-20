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
    {<<"file">>, File} = Qs,
    io:format("[INFO] File string: ~p~n", [File]),

   
    case master_db:get_file(Username,File) of
        {ok, Chuncks} ->
            io:format("[INFO] Chuncks: ~p~n", [Chuncks]),
            {user_file, {_, _}, NumChunks} = Chuncks,
            Json = get_chunks_as_json(Username, File, NumChunks, SecretKey),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, Reason} ->
        cowboy_req:reply(500, #{}, Reason, Req)
    end;
handle(_, Req, _, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).

get_chunks_as_json(Username, FileName, Chuncks, SecretKey) ->
    %% Recupera i record dei chunk dalla query
    Records = master_db:get_chunks(Username, FileName, Chuncks),
    io:format("[INFO] Records: ~p~n", [Records]),
    %% Trasforma ogni record in una mappa
    Files = chunk_to_map(Records, [], SecretKey),
    jiffy:encode(Files).


chunk_to_map([[{chunk, {_Username, _Filename, ChunkPosition}, ChunkName, Nodes}]|Tail], Acc, SecretKey) ->
    TokenChunkName = jwt:encode_file_name(ChunkName, SecretKey),
    IP = case Nodes of
            [] -> undefined;
            [FirstNode | _] -> FirstNode
            end,
    IpFormatted = node_to_url(IP),
    io:format("[INFO] IP: ~p~n", [IpFormatted]),
    IpFormattedBin = list_to_binary(IpFormatted),
    Map = #{<<"ip">> => IpFormattedBin,
            <<"chunkName">> => TokenChunkName,
            <<"chunkPosition">> => ChunkPosition},
    chunk_to_map(Tail, [Map | Acc], SecretKey);
chunk_to_map([], Acc, _) ->
    %% Se vuoi mantenere l'ordine originale, puoi fare lists:reverse(Acc)
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
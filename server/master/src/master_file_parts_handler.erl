-module(master_file_parts_handler).
-export([init/2,get_chunks_as_json/3]).

-behaviour(cowboy_handler).

init(Req, Opts) ->
    [#{username := Username}] = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req,Username),
    {ok, Req2, Opts}.


handle(<<"GET">>, Req, Username) ->
    [Qs] = cowboy_req:parse_qs(Req),
    io:format("[INFO] Query string: ~p~n", [Qs]),
    {<<"file">>, File} = Qs,
    io:format("[INFO] File string: ~p~n", [File]),

   
    case master_db:get_file(Username,File) of
        {ok, Chuncks} ->
            io:format("[INFO] Chuncks: ~p~n", [Chuncks]),
            {user_file, {_, _}, NumChunks} = Chuncks,
            Json = get_chunks_as_json(Username, File, NumChunks),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, Reason} ->
        cowboy_req:reply(500, #{}, Reason, Req)
    end;
handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).



get_chunks_as_json(Username, FileName, Chuncks) ->
    %% Recupera i record dei chunk dalla query
    Records = master_db:get_chunks(Username, FileName, Chuncks),
    io:format("[INFO] Records: ~p~n", [Records]),
    %% Trasforma ogni record in una mappa
    Files = chunk_to_map(Records, []),
    jiffy:encode(Files).


chunk_to_map([[{chunk, {_Username, _Filename, ChunkPosition}, ChunkName, Nodes}]|Tail], Acc) ->
    IP = case Nodes of
            [] -> undefined;
            [FirstNode | _] -> FirstNode
            end,
    Map = #{<<"ip">> => IP,
            <<"chunkName">> => ChunkName,
            <<"chunkPosition">> => ChunkPosition},
    chunk_to_map(Tail, [Map | Acc]);
chunk_to_map([], Acc) ->
    %% Se vuoi mantenere l'ordine originale, puoi fare lists:reverse(Acc)
    lists:reverse(Acc).
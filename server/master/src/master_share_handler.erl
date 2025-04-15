-module(master_share_handler).
-export([init/2]).
-behaviour(cowboy_handler).

-record(file_shared, {username, file_id}).

init(Req, Opts) ->
    [#{username := Username}] = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle_request(Method, Req, Username),
    {ok, Req2, Opts}.

handle_request(<<"POST">>, Req, Username) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Request body: ~p~n", [Body]),
            process_body(Body, Req2, Username);
        {error, Reason} ->
            io:format("[DEBUG] Failed to read body: ~p~n", [Reason]),
            ErrorMsg = io_lib:format("Unable to read body: ~p", [Reason]),
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;
handle_request(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

process_body(Body, Req, Username) ->
    case jiffy:decode(Body, [return_maps]) of
        Json when is_map(Json) ->
            process_json(Json, Req, Username);
        _Other ->
            io:format("[DEBUG] Invalid JSON: ~p~n", [Body]),
            cowboy_req:reply(400, #{}, <<"Invalid JSON body">>, Req)
    end.

process_json(Json, Req, Username) ->
    case json_to_file_shared(Json) of
        #file_shared{} = FileToShare ->
            process_file_share(FileToShare, Req, Username);
        {error, ErrorMsg} ->
            cowboy_req:reply(400, #{}, ErrorMsg, Req)
    end.

json_to_file_shared(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"fileID">>, Map)} of
        {true, true} ->
            FileShared = #file_shared{
                username = maps:get(<<"username">>, Map),
                file_id = maps:get(<<"fileID">>, Map)
            },
            io:format("[INFO] Parsed file shared: ~p~n", [FileShared]),
            FileShared;
        _ ->
            {error, <<"Missing parameters">>}
    end.

process_file_share(FileToShare, Req, RequestUsername) ->
    case verify_file(FileToShare#file_shared.file_id, RequestUsername) of
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req);
        {false, _, _} ->
            cowboy_req:reply(404, #{}, <<"File not found">>, Req);
        {true, FileName, NumChunks} ->
            master_db:insert_file(FileToShare#file_shared.username, FileName, 
                                  FileToShare#file_shared.file_id, NumChunks)
    end.

verify_file(FileID, Username) ->
    io:format("~p ~p~n", [FileID, Username]),
    case master_db:get_file(Username, FileID) of 
        {ok, {user_file, {OwnerUsername, FileName}, FileID, NumChunks}} ->
            {Username =:= OwnerUsername, FileName, NumChunks};
        {error, Reason} -> 
            {error, Reason}
    end.

-module(master_share_handler).
-export([init/2]).

-behaviour(cowboy_handler).

-record(file_shared, {username, file_id}).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    [#{username := Username}] = Opts,
    Req2 = handle(Method, Req, Username),
    {ok, Req2, Opts}.

handle(<<"POST">>, Req, Username) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Body of the request: ~p~n", [Body]),
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    io:format("[DEBUG] chepalleeee"),
                    case map_to_file_shared(Json) of
                        #file_shared{} = FileToShare ->
                            case check_file_exists(FileToShare#file_shared.file_id, Username) of
                                {error, Reason} ->
                                    cowboy_req:reply(500, #{}, Reason, Req2);
                                {false, _, _} ->
                                    cowboy_req:reply(404, #{}, <<"File not found">>, Req2);
                                {true, FileName, NumChunks} ->
                                    master_db:insert_file(FileToShare#file_shared.username, FileName, FileToShare#file_shared.file_id, NumChunks)
                            end;
                        {error, ErrorMsg} ->
                            cowboy_req:reply(400, #{}, ErrorMsg, Req2)
                    end;
                _Other ->
                    io:format("[DEBUG] qualcosa non va ~p",[_Other]),
                    cowboy_req:reply(400, #{}, <<"Body non valido">>, Req2)
            end;
        {error, Reason} ->
            io:format("[DEBUG] qualcosa non va per niente ~p",[Reason]),
            ErrorMsg = io_lib:format("impossible to read the body: ~p", [Reason]),
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;
handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"Metodo non consentito">>, Req).


map_to_file_shared(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"fileID">>, Map)} of
        {true, true} ->
            io:format("[INFO] parsed: ~p~n", [#file_shared{username = maps:get(<<"username">>, Map), 
            file_id =  maps:get(<<"fileID">>, Map)}]),
            #file_shared{username = maps:get(<<"username">>, Map), 
                file_id =  maps:get(<<"fileID">>, Map)};
        _ ->
            {error, <<"missing parameters">>}
    end.

check_file_exists(FileID, Username) ->
    case master_db:get_file(FileID) of 
        {ok, {user_file, {UsernameOwner, FileName}, FileID, NumChunks}} ->
            {Username =:= UsernameOwner, FileName, NumChunks};
        {error, Reason} -> 
            {error, Reason}
    end.
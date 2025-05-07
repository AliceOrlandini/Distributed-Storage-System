-module(master_get_files_handler).
-export([init/2]).
-behaviour(cowboy_handler).

init(Req, Opts) ->
    [#{username := Username}] = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle_request(Method, Req, Username),
    {ok, Req2, Opts}.

handle_request(<<"GET">>, Req, Username) ->
    case master_db:get_files(Username) of
        {ok, Files} ->
            send_files_response(Files, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"not found">>, Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req)
    end;
handle_request(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).

send_files_response(Files, Req) ->
    FileList = build_files_list(Files),
    {ok, JsonResponse} = jsx:encode(#{files => FileList}),
    io:format("Get files Json: ~p~n", [JsonResponse]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Req).

build_files_list(Files) ->
    lists:map(fun convert_file/1, Files).

convert_file({user_file, {_Username, Filename}, FileID, _NumChunks}) ->
    #{fileName => Filename, fileID => convert_file_id(FileID)}.

convert_file_id(FileID) when is_binary(FileID) ->
    FileID;
convert_file_id(FileID) when is_list(FileID) ->
    list_to_binary(lists:flatten(FileID)).

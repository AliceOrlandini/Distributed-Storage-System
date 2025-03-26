-module(master_get_files_handler).
-export([init/2]).

-behaviour(cowboy_handler).

init(Req, Opts) ->
    [#{username := Username}] = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req, Username),
    {ok, Req2, Opts}.


handle(<<"GET">>, Req, Username) ->
    case master_db:get_files(Username) of
        {ok, Files} ->
            Filenames = get_files_name(Files, []),
            Json = jsx:encode(
              #{ files => Filenames }
            ),
            io:format("Get files Json: ~p~n", [Json]),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req)
    end;    
handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).


get_files_name([{user_file, {_Username, Filename}, FileID, _NumChunks}|Tail], Acc) ->
    FileIDStr = case FileID of
        Bin when is_binary(Bin) ->
            Bin;                                   % <<"...">>
        Str when is_list(Str) ->
            list_to_binary(lists:flatten(Str))   % converte "..." in <<"...">>
    end,
    get_files_name(Tail, [#{fileName => Filename, fileID => FileIDStr}|Acc]);
get_files_name([],Acc) ->
    Acc.
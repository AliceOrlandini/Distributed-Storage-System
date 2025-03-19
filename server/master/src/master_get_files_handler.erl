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
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req);
        {error, Reason} ->
            cowboy_req:reply(500, #{}, Reason, Req)
    end;    
handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).


get_files_name([{user_file, {_Username, Filename}, _NumChunks}|Tail], Acc) ->
    get_files_name(Tail, [Filename|Acc]);
get_files_name([],Acc) ->
    Acc.
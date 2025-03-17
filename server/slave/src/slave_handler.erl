-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req,Opts),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req, State) ->
    SecretKey = maps:get(secret_key, State),
    %%%%%% TO REMOVE %%%%%%
    % TokenTest = jwt:encode(#{file_name =><<"file1.txt">>}, SecretKey),
    % io:format("TokenTest: ~p~n", [TokenTest]),
    %%%%%% TO REMOVE %%%%%% 
    % the token is in the body of the request { "token": "token_value" }
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case extract_token(Json) of
                        {ok, ExtractedToken} ->
                            DecodedToken = jwt:decode(ExtractedToken, SecretKey),
                            case DecodedToken of
                                {ok, Claims} ->
                                    FileName = maps:get(file_name, Claims, undefined),
                                    case FileName of
                                        undefined ->
                                            cowboy_req:reply(400, #{}, <<"File name not found inside the token">>, Req2);
                                        _ ->
                                            case retrieve_file:retrieve_file(binary_to_list(FileName), node()) of
                                                {ok, FileContent} ->
                                                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/octet-stream">>}, FileContent, Req);
                                                {error, file_not_found} ->
                                                    ErrorMsg = <<"File not found">>,
                                                    cowboy_req:reply(404, #{}, ErrorMsg, Req);
                                                {error, Reason} ->
                                                    ErrorMsg = lists:flatten(io_lib:format("Errore: ~p", [Reason])),
                                                    cowboy_req:reply(500, #{}, ErrorMsg, Req)
                                            end
                                    end;
                                {error, Reason} ->
                                    ErrorBody = io_lib:format("~p", [Reason]),
                                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req2)
                            end;
                        {error, ErrorMsg} ->
                            cowboy_req:reply(400, #{}, ErrorMsg, Req2)
                    end;
                _Other ->
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Body not valid">>, Req2)
            end;
        {error, Reason} ->
            ErrorBody = io_lib:format("~p", [Reason]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req)
    end;
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).
    
extract_token(Map) ->
    case {maps:is_key(<<"token">>, Map)} of
        {true} ->
            ExtractedToken = maps:get(<<"token">>, Map),
            {ok, ExtractedToken};
        _ ->
            {error, <<"Missing Parameters">>}
    end.

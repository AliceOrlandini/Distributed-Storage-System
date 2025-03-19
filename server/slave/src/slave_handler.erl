-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    #{secret_key := SecretKey} = Opts,
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req, SecretKey),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req, SecretKey) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case extract_token(Json) of
                        {ok, ExtractedToken} ->
                            DecodedToken = jwt:decode(ExtractedToken, SecretKey),
                            case DecodedToken of
                                {ok, FileName} ->
                                    case FileName of
                                        undefined ->
                                            cowboy_req:reply(400, #{}, <<"file name not found inside the token">>, Req2);
                                        _ ->
                                            case retrieve_file:retrieve_file(binary_to_list(FileName), node()) of
                                                {ok, FileContent} ->
                                                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/octet-stream">>}, FileContent, Req);
                                                {error, file_not_found} ->
                                                    ErrorMsg = <<"file not found">>,
                                                    cowboy_req:reply(404, #{}, ErrorMsg, Req);
                                                {error, Reason} ->
                                                    ErrorMsg = lists:flatten(io_lib:format("error during file retrieval: ~p", [Reason])),
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
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"body not valid">>, Req2)
            end;
        {error, Reason} ->
            ErrorBody = io_lib:format("~p", [Reason]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req)
    end;
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"not found">>, Req).
    
extract_token(Map) ->
    case {maps:is_key(<<"token">>, Map)} of
        {true} ->
            ExtractedToken = maps:get(<<"token">>, Map),
            {ok, ExtractedToken};
        _ ->
            {error, <<"missing parameters">>}
    end.

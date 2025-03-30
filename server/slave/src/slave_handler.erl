-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    #{secret_key := SecretKey} = Opts,
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req, SecretKey),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req, SecretKey) ->
    [Qs] = cowboy_req:parse_qs(Req),
    {<<"token">>, Token} = Qs,
    DecodedToken = jwt:decode(Token, SecretKey),
    case DecodedToken of
        {ok, FileName} ->
            case FileName of
                undefined ->
                    cowboy_req:reply(400, #{}, <<"file name not found inside the token">>, Req);
                _ ->
                    case retrieve_file:retrieve_file(binary_to_list(FileName), node()) of
                        {ok, FileContent} ->
                            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, FileContent, Req),
                            slave_db:update_status_field(pending_requests, -1);
                        {error, file_not_found} ->
                            ErrorMsg = <<"file not found">>,
                            cowboy_req:reply(404, #{}, ErrorMsg, Req);
                        {error, Reason} ->
                            ErrorMsg = lists:flatten(io_lib:format("error during file retrieval: ~p", [Reason])),
                            cowboy_req:reply(500, #{}, ErrorMsg, Req),
                            slave_db:update_status_field(pending_requests, -1)
                    end
            end;
        {error, Reason} ->
            ErrorBody = io_lib:format("~p", [Reason]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req)
    end;
handle(_, Req, _) ->
    cowboy_req:reply(404, #{}, <<"not found">>, Req).

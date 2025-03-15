-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Reply = handle(Method, Req),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>},
                                <<"Authorization header missing">>, Req);
        AuthHeader ->
            Token = extract_token(AuthHeader),
            case jwt:decode(Token, <<"abcd">>) of
                {error, Reason} ->
                    %% Convertiamo Reason in iodata (stringa) per la risposta
                    ErrorBody = io_lib:format("~p", [Reason]),
                    cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req);
                Claims ->
                    cowboy_req:reply(200, #{}, <<"ok">>, Req)
            end
    end;
handle(_, Req) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).
    

extract_token(AuthHeader) ->
    BearerPrefix = <<"Bearer ">>,
    case binary:match(AuthHeader, BearerPrefix) of
        {0, _Length} ->
            % remove the Bearer prefix from the token
            TokenLen = byte_size(AuthHeader) - byte_size(BearerPrefix),
            binary:part(AuthHeader, byte_size(BearerPrefix), TokenLen);
        nomatch ->
            % if the Bearer prefix is missing, return an empty token
            <<"">>
    end.

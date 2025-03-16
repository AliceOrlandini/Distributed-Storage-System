-module(slave_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    SecretKey = maps:get(secret_key, Opts),
    Reply = handle(Method, Req, SecretKey),
    {ok, Reply, Opts}.

handle(<<"GET">>, Req, SecretKey) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>},
                                <<"Authorization header missing">>, Req);
        AuthHeader ->
            % Payload = #{<<"sub">> => <<"1234567890">>, <<"name">> => <<"John Doe">>, <<"admin">> => true},
            % Test = jwt:encode(hs256, Payload, SecretKey, [{<<"auth">>, <<"Bearer Token">>}]),
            % io:format("Test: ~p~n", [Test]),
            Token = extract_token(AuthHeader),
            case jwt:decode(Token, SecretKey) of
                {error, Reason} ->
                    ErrorBody = io_lib:format("~p", [Reason]),
                    cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req);
                Claims ->
                    io:format("Claims: ~p~n", [Claims]),
                    cowboy_req:reply(200, #{}, <<"ok">>, Req)
            end
    end;
handle(_, Req, _) ->
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

-module(master_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    #{secret_key := SecretKey, dispatch := Dispatch} = Env,
    Root = cowboy_req:path(Req),
    io:format("Root: ~p~n", [Root]),
    case handle(Root,Req, SecretKey) of
        ok ->
            io:format("ok tralla baralla~n", []),
            {ok, Req, Env};
        {stop, NewReq} ->
            {stop, NewReq};
        {ok, Username} ->
            NewEnv = Env#{username => Username,dispatch := Dispatch},
            {ok, Req, NewEnv}
    end.




handle(<<"/registration">>,_,_) -> 
    io:format("registration~n", []),
    ok;
handle(<<"/login">>,_,_) -> ok;
handle(_Path, Req, SecretKey) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>},
                                <<"Authorization header missing">>, Req),
            {stop, NewReq};
        AuthHeader ->
            Token = extract_token(AuthHeader),
            case jwt:decode(Token, SecretKey) of
                {error, _} ->
                    NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, "error", Req),
                    {stop, NewReq};
                {ok, Username} ->
                    case master_db:get_user(Username) of
                        {error, _} ->
                            NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, "error", Req),
                            {stop, NewReq};
                        {ok, _User} ->
                            {ok, Username}
                    end
                end
    end.

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
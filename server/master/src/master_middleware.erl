-module(master_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    #{dispatch := [{'_',
                     _,
                     [{_Paths, _Method, _Handler, Options}]
                    }]} = Env,
    SecretKey = maps:get(secret_key, Options),
    case handle(Req, SecretKey) of
        {stop, NewReq} ->
            {stop, NewReq};
        {ok, FileName} ->
            NewOptions = Options#{file_name => FileName},
            OldDispatch = maps:get(dispatch, Env),
            NewEnv = Env#{dispatch => [{'_',
                element(2, hd(OldDispatch)),
                [{_Paths, _Method, _Handler, NewOptions}]
            }]},
            {ok, Req, NewEnv}
    end.

handle(Req, SecretKey) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>},
                                <<"Authorization header missing">>, Req),
            {stop, NewReq};
        AuthHeader ->
            Token = extract_token(AuthHeader),
            case jwt:decode(Token, SecretKey) of
                {error, Reason} ->
                    ErrorBody = io_lib:format("~p", [Reason]),
                    NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>}, ErrorBody, Req),
                    {stop, NewReq};
                {ok, Username} ->
                    {ok, Username}
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
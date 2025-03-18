-module(master_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    #{secret_key := SecretKey, dispatch := Dispatch} = Env,
    Root = cowboy_req:path(Req),
    case handle(Root, Req, SecretKey) of
        ok ->
            {ok, Req, Env};
        {stop, NewReq} ->
            {stop, NewReq};
        {ok, Username} ->
            UpdatedDispatch = update_dispatch(Dispatch, Root, Username),
            NewEnv = Env#{username => Username, dispatch := UpdatedDispatch},
            {ok, Req, NewEnv}
    end.

handle(<<"/registration">>, _, _) -> 
    ok;

handle(<<"/login">>, _, _) -> 
    ok;

handle(_Path, Req, SecretKey) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            NewReq = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">>},
                                <<"Authorization header missing">>, Req),
            {stop, NewReq};
        AuthHeader ->
            io:format("AuthHeader: ~p~n", [AuthHeader]),
            Token = extract_token(AuthHeader),
            io:format("Token: ~p~n", [Token]),
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

update_dispatch(Dispatch, Root, Username) when is_list(Dispatch) ->
    lists:map(fun({Host, Prefix, Routes}) ->
                        {Host, Prefix, update_routes(Routes, Root, Username)}
                end, Dispatch);

update_dispatch({Host, Prefix, Routes}, Root, Username) ->
    {Host, Prefix, update_routes(Routes, Root, Username)}.

update_routes(Routes, Root, Username) ->
    NormalizedRoot = normalize_path(Root),
    lists:map(
        fun({Path, PrefixPath, Handler, Options}) ->
            % convert Path to a list of binaries
            NormalizedPath = iolist_to_binary(Path),
            if
                NormalizedPath == NormalizedRoot ->
                    % if the path matches the root, add the username to the options
                    {Path, PrefixPath, Handler, Options ++ [#{username => Username}]};
                true ->
                    {Path, PrefixPath, Handler, Options}
            end
        end,
        Routes
    ).
    
normalize_path(Root) ->
    % if the path starts with a slash, remove the trailing slash
    case binary:match(Root, <<"/">>) of
        {0, _Len} ->
            binary:part(Root, 1, byte_size(Root)-1);
        _ ->
            Root
    end.
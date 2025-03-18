-module(master_login_handler).
-export([init/2]).

-behaviour(cowboy_handler).

-record(user, {username, password}).

init(Req, Opts) ->
    #{
        secret_key := SecretKey
    } = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req,SecretKey),
    {ok, Req2, Opts}.


handle(<<"POST">>, Req,SecretKey) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Body of the request: ~p~n", [Body]),
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case map_to_user(Json) of
                        #user{} = User ->
                            % get the user information from the mnesia database
                            case master_db:get_user(User#user.username) of
                                {ok, UserFetch} ->        
                                    case chek_password(User#user.password, UserFetch#user.password) of
                                        true ->
                                            Token = jwt:encode_username(User#user.username, SecretKey),
                                            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},  [<<"{\"token\":\"">>, Token, <<"\"}">>], Req2);    
                                        false ->
                                            cowboy_req:reply(400, #{}, <<"user not registered">>, Req2)
                                    end;


                                {error, not_found} ->
                                    cowboy_req:reply(400, #{}, <<"error">>, Req2)
                            end;
                        {error, ErrorMsg} ->
                            cowboy_req:reply(400, #{}, ErrorMsg, Req2)
                    end;
                _Other ->
                    cowboy_req:reply(400, #{}, <<"body not valid">>, Req2)
            end;
        {error, Reason} ->
            ErrorMsg = ["impossible to read the body: ", io_lib:format("~p", [Reason])],
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;

handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"method not allowed">>, Req).

map_to_user(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"password">>, Map)} of
        {true, true} ->
            Username = maps:get(<<"username">>, Map),
            Password = maps:get(<<"password">>, Map),
            #user{username = Username, password = Password};
        _ ->
            {error, <<"missing parameters">>}
    end.

chek_password(PasswordReceived, Password) ->
    crypto:hash(sha256, PasswordReceived) == Password.
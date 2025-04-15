-module(master_login_handler).
-export([init/2]).
-behaviour(cowboy_handler).

-record(user, {username, password}).

init(Req, Opts) ->
    #{secret_key := SecretKey} = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle_request(Method, Req, SecretKey),
    {ok, Req2, Opts}.

handle_request(<<"POST">>, Req, SecretKey) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Request body: ~p~n", [Body]),
            process_body(Body, Req2, SecretKey);
        {error, Reason} ->
            ErrorMsg = io_lib:format("Impossible to read the body: ~p", [Reason]),
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;
handle_request(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

process_body(Body, Req, SecretKey) ->
    case jiffy:decode(Body, [return_maps]) of
        Json when is_map(Json) ->
            process_json(Json, Req, SecretKey);
        _Other ->
            cowboy_req:reply(400, #{}, <<"Invalid body">>, Req)
    end.

process_json(Json, Req, SecretKey) ->
    case map_to_user(Json) of
        #user{} = User ->
            perform_login(User, Req, SecretKey);
        {error, ErrorMsg} ->
            cowboy_req:reply(400, #{}, ErrorMsg, Req)
    end.

perform_login(User, Req, SecretKey) ->
    Username = User#user.username,
    case master_db:get_user(Username) of
        {ok, UserRecord} ->
            case check_password(User#user.password, UserRecord#user.password) of
                true ->
                    Token = master_jwt:encode_username(Username, SecretKey),
                    ResponseBody = [<<"{\"token\":\"">>, Token, <<"\"}">>],
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, ResponseBody, Req);
                false ->
                    cowboy_req:reply(400, #{}, <<"User not registered">>, Req)
            end;
        {error, not_found} ->
            cowboy_req:reply(400, #{}, <<"Error">>, Req)
    end.

map_to_user(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"password">>, Map)} of
        {true, true} ->
            Username = maps:get(<<"username">>, Map),
            Password = maps:get(<<"password">>, Map),
            #user{username = Username, password = Password};
        _ ->
            {error, <<"Missing parameters">>}
    end.

check_password(PasswordReceived, StoredPassword) ->
    crypto:hash(sha256, PasswordReceived) == StoredPassword.

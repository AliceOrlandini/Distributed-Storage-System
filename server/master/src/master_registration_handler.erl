-module(master_registration_handler).
-export([init/2]).
-behaviour(cowboy_handler).

-record(user, {username, password}).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Req2 = handle_request(Method, Req),
    {ok, Req2, Opts}.

handle_request(<<"POST">>, Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Request body: ~p~n", [Body]),
            process_request_body(Body, Req2);
        {error, Reason} ->
            ErrorMsg = io_lib:format("Failed to read body: ~p", [Reason]),
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;
handle_request(_, Req) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

process_request_body(Body, Req) ->
    case jiffy:decode(Body, [return_maps]) of
        Json when is_map(Json) ->
            process_json(Json, Req);
        _Other ->
            cowboy_req:reply(400, #{}, <<"Invalid JSON body">>, Req)
    end.

process_json(Json, Req) ->
    case map_to_user(Json) of
        #user{} = User ->
            register_user(User, Req);
        {error, ErrorMsg} ->
            cowboy_req:reply(400, #{}, ErrorMsg, Req)
    end.

register_user(User, Req) ->
    case master_db:insert_user(User#user.username, User#user.password) of
        {ok, inserted} ->
            cowboy_req:reply(200, #{}, <<"registration successful">>, Req);
        {aborted, Reason} ->
            ErrorMsg = io_lib:format("Error during registration: ~p", [Reason]),
            cowboy_req:reply(500, #{}, list_to_binary(ErrorMsg), Req)
    end.

map_to_user(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"password">>, Map)} of
        {true, true} ->
            Username = maps:get(<<"username">>, Map),
            Password = maps:get(<<"password">>, Map),
            case check_username(Username) of
                true ->
                    {error, <<"username already exists">>};
                false ->
                    case check_length_password(Password) of
                        true ->
                            #user{username = Username, password = Password};
                        false ->
                            {error, <<"password too short">>}
                    end
            end;
        _ ->
            {error, <<"missing parameters">>}
    end.

check_username(Username) ->
    case master_db:get_user(Username) of
        {ok, _} -> true;
        {error, not_found} -> false;
        {error, {no_exists, user}} -> false;
        _Other ->
            io:format("[ERROR] Error checking username: ~p~n", [Username]),
            false
    end.

check_length_password(Password) when is_binary(Password) ->
    byte_size(Password) >= 8;
check_length_password(Password) when is_list(Password) ->
    length(Password) >= 8.

-module(master_registration_handler).
-export([init/2]).

-behaviour(cowboy_handler).

-record(user, {username, password}).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req),
    {ok, Req2, Opts}.

handle(<<"POST">>, Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("[INFO] Body of the request: ~p~n", [Body]),
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case map_to_user(Json) of
                        #user{} = User ->
                            % insert the user in the mnesia database
                            case master_db:insert_user(User#user.username, User#user.password) of
                                {ok, inserted} ->
                                    cowboy_req:reply(200, #{}, <<"registration successful">>, Req2);
                                {aborted, Reason} ->
                                    ErrorMsg = io_lib:format("error during registration: ~p", [Reason]),
                                    cowboy_req:reply(500, #{}, list_to_binary(ErrorMsg), Req2)
                            end;
                        {error, ErrorMsg} ->
                            cowboy_req:reply(400, #{}, ErrorMsg, Req2)
                    end;
                _Other ->
                    cowboy_req:reply(400, #{}, <<"Body non valido">>, Req2)
            end;
        {error, Reason} ->
            ErrorMsg = io_lib:format("impossible to read the body: ~p", [Reason]),
            cowboy_req:reply(400, #{}, list_to_binary(ErrorMsg), Req)
    end;

handle(_, Req) ->
    cowboy_req:reply(405, #{}, <<"Metodo non consentito">>, Req).

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
        {error,{no_exists,user}} -> false;
        _Other -> 
            io:format("[ERROR] error during the check of the username ~p~n", [Username]),
            false
    end.

check_length_password(Password) when is_binary(Password) ->
    byte_size(Password) >= 8;
check_length_password(Password) when is_list(Password) ->
    length(Password) >= 8.

-module(master_registration_handler).
-export([init/2]).

-behaviour(cowboy_handler).

-record(user, {username, password}).

%% Entry point del Cowboy handler
init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req),
    {ok, Req2, Opts}.

%% Gestione della richiesta POST
handle(<<"POST">>, Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("body ~p~n", [Body]),
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case map_to_user(Json) of
                        #user{} = User ->
                            %% Scrive l'utente in Mnesia e gestisce l'esito della transazione
                            case master_db:insert_user(User#user.username, User#user.password) of
                                {ok, inserted} ->
                                    cowboy_req:reply(200, #{}, <<"Registrazione completata">>, Req2);
                                {aborted, Reason} ->
                                    ErrorMsg = <<"Errore nella transazione: ">> ++
                                               list_to_binary(io_lib:format("~p", [Reason])),
                                    cowboy_req:reply(500, #{}, ErrorMsg, Req2)
                            end;
                        {error, ErrorMsg} ->
                            cowboy_req:reply(400, #{}, ErrorMsg, Req2)
                    end;
                _Other ->
                    cowboy_req:reply(400, #{}, <<"Body non valido">>, Req2)
            end;
        {error, Reason} ->
            ErrorMsg = <<"Impossibile leggere il body: ">> ++
                       list_to_binary(io_lib:format("~p", [Reason])),
            cowboy_req:reply(400, #{}, ErrorMsg, Req)
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
                    {error, <<"Username già esistente">>};
                false ->
                    case check_length_password(Password) of
                        true ->
                            #user{username = Username, password = Password};
                        false ->
                            {error, <<"Password troppo corta">>}
                    end
            end;
        _ ->
            {error, <<"Parametri mancanti">>}
    end.

check_username(Username) ->
    case master_db:get_user(Username) of
        {ok, _} -> true;
        {error, not_found} -> false;
        {error,{no_exists,user}} -> false;
        _Other -> 
            io:format("Errore nella ricerca dell'utente ~p~n", [_Other]),
            false
    end.

check_length_password(Password) when is_binary(Password) ->
    byte_size(Password) >= 8;
check_length_password(Password) when is_list(Password) ->
    length(Password) >= 8.

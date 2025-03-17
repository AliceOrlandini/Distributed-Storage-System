-module(master_login_handler).
-export([init/2]).

-behaviour(cowboy_handler).

-record(user, {username, password}).

%% Entry point del Cowboy handler
init(Req, Opts) ->
    #{
        secret_key := SecretKey
    } = Opts,
    Method = cowboy_req:method(Req),
    Req2 = handle(Method, Req,SecretKey),
    {ok, Req2, Opts}.

%% Gestione della richiesta POST
handle(<<"POST">>, Req,SecretKey) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            io:format("body ~p~n", [Body]),
            case jiffy:decode(Body, [return_maps]) of
                Json when is_map(Json) ->
                    case map_to_user(Json) of
                        #user{} = User ->
                            %% Scrive l'utente in Mnesia e gestisce l'esito della transazione
                            case master_db:get_user(User#user.username) of
                                {ok, UserFetch} ->        
                                    case chek_password(User#user.password, UserFetch#user.password) of
                                        true ->
                                            Token = jwt:encode_username(User#user.username,SecretKey),
                                            cowboy_req:reply(200, #{},  [<<"{\"token\":\"">>, Token, <<"\"}">>], Req2);    
                                        false ->
                                            cowboy_req:reply(400, #{}, <<"error">>, Req2)
                                    end;

                                    
                                {error, not_found} ->
                                    cowboy_req:reply(400, #{}, <<"error">>, Req2)
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

handle(_, Req, _) ->
    cowboy_req:reply(405, #{}, <<"Metodo non consentito">>, Req).

map_to_user(Map) ->
    case {maps:is_key(<<"username">>, Map), maps:is_key(<<"password">>, Map)} of
        {true, true} ->
            Username = maps:get(<<"username">>, Map),
            Password = maps:get(<<"password">>, Map),
            #user{username = Username, password = Password};
        _ ->
            {error, <<"Parametri mancanti">>}
    end.

chek_password(PasswordReceived, Password) ->
    crypto:hash(sha256, PasswordReceived) == Password.
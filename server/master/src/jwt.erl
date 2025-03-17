-module(jwt).
-export([encode/2, decode/2]).

%% record payload non usato direttamente in questo modulo,
%% ma i claim sono costruiti come lista di tuple
%% per compatibilità con jwerl.

encode(FileName, Key) ->
    NormKey = crypto:hash(sha256, Key),
    %% Aggiungiamo il claim exp con scadenza tra 864000 secondi (10 giorni)
    Claims = [{file_name, FileName}, {exp, now_secs() + 1}],
    %% Firma il token con HS256 usando la chiave normalizzata
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.

decode(Token, Key) ->
    NormKey = crypto:hash(sha256, Key),
    case jwerl:verify(Token, hs256, NormKey) of
         {ok, Claims} ->
             FileName = maps:get(file_name, Claims, undefined),
             Exp = maps:get(exp, Claims, undefined),
             case {FileName, Exp} of
                  {undefined, _} ->
                      {error, file_name_not_found};
                  {_, undefined} ->
                      {error, exp_not_found};
                  {_FileName, _Exp} ->
                      Now = now_secs(),
                      if
                          Now > Exp ->
                              {error, token_expired};
                          true ->
                              {ok, FileName}
                      end
             end;
         {error, Reason} ->
             {error, Reason}
    end.

%% now_secs/0
%% Restituisce il timestamp corrente in secondi.
now_secs() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

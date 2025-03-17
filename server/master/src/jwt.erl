-module(jwt).
-export([encode_file_name/2,encode_username/2, decode/2]).

%% record payload non usato direttamente in questo modulo,
%% ma i claim sono costruiti come lista di tuple
%% per compatibilità con jwerl.

encode_file_name(FileName, Key) ->
    NormKey = crypto:hash(sha256, Key),
    %% Aggiungiamo il claim exp con scadenza tra 5 minuti
    Claims = [{file_name, FileName}, {exp, now_secs() + 300}],
    %% Firma il token con HS256 usando la chiave normalizzata
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.

encode_username(Username, Key) ->
    NormKey = crypto:hash(sha256, Key),
    %% Aggiungiamo il claim exp con scadenza tra 864000 secondi (10 giorni)
    Claims = [{file_name, Username}],
    %% Firma il token con HS256 usando la chiave normalizzata
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.


decode(Token, Key) ->
    NormKey = crypto:hash(sha256, Key),
    case jwerl:verify(Token, hs256, NormKey) of
         {ok, Claims} ->
             case maps:get(file_name, Claims, undefined) of
                {undefined} ->
                    {error, file_name_not_found};
                {Username} ->
                    {ok, Username}
             end;
         {error, Reason} ->
             {error, Reason}
    end.


%% now_secs/0
%% Restituisce il timestamp corrente in secondi.
now_secs() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

-module(jwt).
-export([encode_file_name/2,encode_username/2, decode/2]).

encode_file_name(FileName, Key) ->
    NormKey = crypto:hash(sha256, Key),
    % add claim exp with expiration in 3000 seconds (50 minutes)
    Claims = [{file_name, FileName}, {exp, now_secs() + 3000}],
    % sign the token with HS256 using the normalized key
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.

encode_username(Username, Key) ->
    NormKey = crypto:hash(sha256, Key),
    % add claim exp with expiration in 864000 seconds (10 days)
    Claims = [{file_name, Username}, {exp, now_secs() + 864000}],
    % sign the token with HS256 using the normalized key
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.

decode(Token, Key) ->
    NormKey = crypto:hash(sha256, Key),
    case jwerl:verify(Token, hs256, NormKey) of
         {ok, Claims} ->
             case maps:get(file_name, Claims, undefined) of
                {undefined} ->
                    {error, file_name_not_found};
                Username ->
                    {ok, Username}
            end;
         {error, Reason} ->
             {error, Reason}
    end.


% this function returns the current time in seconds
now_secs() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

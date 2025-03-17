-module(jwt).
-export([decode/2, encode/2]).

encode(FileName, Key) ->
    NormKey = crypto:hash(sha256, Key),
    % add the expiration time clame with 864000 seconds (10 days)
    Claims = [{file_name, FileName}, {exp, now_secs() + 864000}],
    % sign the token with HS256 using the normalized key
    Jwt = jwerl:sign(Claims, hs256, NormKey),
    Jwt.

decode(Token, Key) ->
    NormKey = crypto:hash(sha256, Key),
    % elp:ignore W0017 (undefined_function)
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
                    if Now > Exp ->
                        {error, token_expired};
                    true ->
                        {ok, FileName}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% returns the current time in seconds
now_secs() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

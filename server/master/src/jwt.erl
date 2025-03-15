-module(jwt).
-export([generate_jwt/1]).

generate_jwt(FileName) ->
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ExpirationTime = CurrentTime + 864000, % ten days
    Claims = #{
        <<"file_name">> => FileName,
        <<"exp">> => ExpirationTime
    },
    {ok, Jwt} = erljwt:sign(<<"private_key">>, <<"HS256">>, Claims),
    Jwt.
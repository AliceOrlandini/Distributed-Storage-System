-module(hash_chunck).

-export([get_hash/1]).


% list_to_integer(binary_to_list(Hex), 16)
-spec get_hash(binary()) -> [binary()].
    
get_hash(Data) when is_binary(Data) ->
    Digest = crypto:hash(sha, Data),
    [binary:encode_hex(Digest)];
get_hash(Data) when is_list(Data) ->
    get_hash(Data, []).

get_hash([], Acc) ->
    Acc;
get_hash([Head|Tail], Acc) ->
    Digest = crypto:hash(sha, Head),
    Hex = binary:encode_hex(Digest),
    get_hash(Tail, Acc ++ [Hex]).
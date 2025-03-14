-module(file_chunks).
-export([devide_into_chunks/2]).

-spec devide_into_chunks(binary(), non_neg_integer()) -> [binary()].

devide_into_chunks(Data, ChunkSize) ->
    devide_into_chunks(Data, ChunkSize, []).

devide_into_chunks(Data, _ChunkSize, Acc) when byte_size(Data) =< _ChunkSize ->
    Acc ++ [Data];
devide_into_chunks(Data, ChunkSize, Acc) when byte_size(Data) > ChunkSize ->
    Chunk = binary:part(Data, 0, ChunkSize),
    Rest = binary:part(Data, ChunkSize, byte_size(Data) - ChunkSize),
    devide_into_chunks(Rest, ChunkSize, Acc ++ [Chunk]).


%%%-------------------------------------------------------------------
%% @doc Handler per la cancellazione di un file lato master
%%%-------------------------------------------------------------------

-module(master_delete_handler).
-behaviour(cowboy_handler).

-include("../include/master_db.hrl").

-export([init/2]).

init(Req, State) ->
    %% Estrai parametri dalla richiesta (file_id e username)
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case jsx:decode(Body, [return_maps]) of
        #{"file_id" := FileID, "username" := Username} ->
            handle_delete(FileID, Username, Req1, State);
        _ ->
            Resp = jsx:encode(#{error => "Invalid parameters"}),
            Req2 = cowboy_req:reply(400, #{"content-type" => "application/json"}, Resp, Req1),
            {ok, Req2, State}
    end.

handle_delete(FileID, Username, Req, State) ->
    %% Controlla se l'utente possiede il file
    case master_db:get_file(Username, FileID) of
        {ok, _} ->
            %% Controlla se altri utenti possiedono il file
            case master_db:has_other_owners(FileID, Username) of
                true ->
                    master_db:remove_user_file(Username, FileID),
                    Resp = jsx:encode(#{result => "User-file relation deleted"}),
                    Req2 = cowboy_req:reply(200, #{"content-type" => "application/json"}, Resp, Req),
                    {ok, Req2, State};
                false ->
                    delete_chunks_and_file(FileID, Username),
                    Resp = jsx:encode(#{result => "File and chunks deleted"}),
                    Req2 = cowboy_req:reply(200, #{"content-type" => "application/json"}, Resp, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Resp = jsx:encode(#{error => "File not found for user"}),
            Req2 = cowboy_req:reply(404, #{"content-type" => "application/json"}, Resp, Req),
            {ok, Req2, State}
    end.

delete_chunks_and_file(FileID, Username) ->
    {ok, [#user_file{num_chuncks = NumChunks}]} = master_db:get_file(Username, FileID),
    Chunks = master_db:get_chunks(FileID, NumChunks),
    send_delete_to_slaves(Chunks),
    master_db:remove_user_file(Username, FileID),
    master_db:remove_chunks(FileID, NumChunks).

send_delete_to_slaves(Chunks) ->
    lists:foreach(
        fun(ChunkList) ->
            case ChunkList of
                [#chunk{chunk_name = ChunkName, nodes = Nodes}] ->
                    lists:foreach(
                        fun(Node) ->
                            rpc:call(Node, save_file, delete_chunk, [ChunkName, Node])
                        end,
                        Nodes
                    );
                _ -> ok
            end
        end,
        Chunks
    ).

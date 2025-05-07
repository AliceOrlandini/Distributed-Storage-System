%%%-------------------------------------------------------------------
%% @doc Handler per la cancellazione di un file lato master
%%%-------------------------------------------------------------------

-module(master_delete_handler).
-behaviour(cowboy_handler).

-include("../include/master_db.hrl").

-export([init/2]).

init(Req, Opts) ->
    %% Prendi fileID dai query params
    Qs0 = cowboy_req:parse_qs(Req),
    Qs = case is_list(Qs0) of
             true -> Qs0;
             false -> [Qs0]
         end,
    [#{username := Username}] = Opts,

    case proplists:get_value(<<"fileID">>, Qs, undefined) of
    undefined ->
        cowboy_req:reply(400, #{}, <<"Missing fileID parameter">>, Req);
    FileID ->
        io:format("[INFO] FileID: ~p~n", [FileID]),
        handle_delete(FileID, Username, Req, Opts)
    end.

handle_delete(FileID, Username, Req, State) ->
    %% Controlla se l'utente possiede il file
    case master_db:get_file(Username, FileID) of
        {ok, _} ->
            %% Controlla se altri utenti possiedono il file
            case master_db:has_other_owners(FileID, Username) of
                true ->
                    master_db:delete_file(Username, FileID),
                    {ok, EncodedResp} = jsx:encode(#{result => "User-file relation deleted"}),
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, EncodedResp, Req),
                    {ok, Req2, State};
                false ->
                    delete_chunks_and_file(FileID, Username),
                    {ok, EncodedResp} = jsx:encode(#{result => "File and chunks deleted"}),
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, EncodedResp, Req),
                    {ok, Req2, State}
            end;
        _ ->
            {ok, EncodedResp} = jsx:encode(#{error => "File not found for user"}),
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, EncodedResp, Req),
            {ok, Req2, State}
    end.

delete_chunks_and_file(FileID, Username) ->
    {ok, #user_file{num_chuncks = NumChunks}} = master_db:get_file(Username, FileID),
    Chunks = master_db:get_chunks(FileID, NumChunks),
    send_delete_to_slaves(Chunks),
    master_db:delete_file(Username, FileID),
    master_db:remove_chunks(FileID, NumChunks).

send_delete_to_slaves(Chunks) ->
    lists:foreach(
        fun(ChunkList) ->
            case ChunkList of
                [#chunk{chunk_name = ChunkName, nodes = Nodes}] ->
                    lists:foreach(
                        fun(Node) ->
                            % elp:ignore W0014 (cross_node_eval)
                            rpc:call(Node, save_file, delete_chunk, [ChunkName, Node])
                        end,
                        Nodes
                    );
                _ -> ok
            end
        end,
        Chunks
    ).

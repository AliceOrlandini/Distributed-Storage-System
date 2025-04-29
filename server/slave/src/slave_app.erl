%%%-------------------------------------------------------------------
%% @doc slave public API
%% @end
%%%-------------------------------------------------------------------

-module(slave_app).

-behaviour(application).

-export([start/2, stop/1]).
-include_lib("common_test/include/ct.hrl").

start(_StartType, _StartArgs) ->
    Port = case os:getenv("PORT") of
        false ->
            5000;
        PortStr ->
            io:format("[INFO] Port: ~p~n", [PortStr]), 
            {PortInt, _} = string:to_integer(PortStr),
            PortInt
    end,
    io:format("[INFO] Starting slave app on port ~p~n", [Port]),  
    SecretKey = case os:getenv("SECRET_KEY") of
        false ->
            "";
        SecretKeyStr ->
            SecretKeyStr
    end,
    init_db(),
    % spawn a new process to handle the connections
    register(slave, spawn(fun loop/0)),
    Dispatch = cowboy_router:compile([
        {'_', [{"/download", slave_handler, #{secret_key => list_to_binary(SecretKey)}}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{
        env => #{dispatch => Dispatch}}).

stop(_State) ->
    ok.


init_db() ->
    mnesia:create_schema([node()]),
    slave_db:create_tables(node()).

loop() ->
    receive
        {file, FileName, FileContent, ReplicationNode} ->
            io:format("[INFO] Received file: ~p~n", [FileName]),
            save_file:save_file(FileName, FileContent, node()),
            io:format("[INFO] Send file to replication node: ~p~n", [ReplicationNode]),
            {slave, ReplicationNode} ! {file, FileName, FileContent},
            loop();
        {file, FileName, FileContent} ->
            io:format("[INFO] Received replicated file: ~p~n", [FileName]),
            save_file:save_file(FileName, FileContent, node()),
            loop();
        {delete_chunk, ChunkName} ->
            io:format("[INFO] Received delete_chunk for: ~p~n", [ChunkName]),
            save_file:delete_chunk(ChunkName, node()),
            loop();
        {status, _ServerIP, ServerPid} -> 
            io:format("[INFO] Received status request.~n"),
            case slave_db:get_status() of
                {ok, Status} ->
                    Pending = maps:get(pending_requests, Status),
                    Possible = maps:get(possible_requests, Status), 
                    io:format("[INFO] Pending: ~p~n", [Pending]),
                    io:format("[INFO] Possible: ~p~n", [Possible]),
                    ServerPid ! {status, Pending, Possible},
                    slave_db:update_status_field(possible_requests, 1);
                {error, not_found} ->
                    io:format("[INFO] Status not found~n");
                {error, Reason} ->
                    io:format("[INFO] Error getting status: ~p~n", [Reason])
            end,
            loop();
        {choose} ->
            io:format("[INFO] Received choose request.~n"),
            slave_db:update_status_field(pending_requests, 1),
            slave_db:update_status_field(possible_requests, -1),
            loop();
        {not_choose} -> 
            io:format("[INFO] Received not choose request.~n"),
            slave_db:update_status_field(possible_requests, -1),
            loop();
        _Other ->
            io:format("[INFO] Unknown messagge.~n"),
            loop()
    end.

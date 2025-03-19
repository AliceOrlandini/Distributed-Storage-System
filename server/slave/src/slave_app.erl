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
            5001;
        PortStr ->
            io:format("[INFO] Port: ~p~n", [PortStr]), 
           {PortInt, _} = string:to_integer(PortStr),
            PortInt
    end,
    io:format("[INFO] Starting slave app on port ~p~n", [Port]),  
    NumReplicas = case application:get_env('Distributed-Storage-System', num_replicas) of
        {ok, N} -> N;
        undefined -> 0
    end,
    SecretKey = case os:getenv("SECRET_KEY") of
        false ->
            "";
        SecretKeyStr ->
            SecretKeyStr
    end,

    IsBootstrap = case application:get_env('Distributed-Storage-System', bootstrap) of
        {ok, B} -> B;
        undefined -> false
    end,
    Nodes = start_nodes(IsBootstrap, NumReplicas, SecretKey),
    io:format("[INFO] Number of replicas: ~p~n", [Nodes]),

    lists:foreach(fun(Node) ->
        io:format("[INFO] Ping ~p: ~p~n", [Node, net_adm:ping(Node)])
    end, Nodes),

    % spawn a new process to handle the connections
    register(slave, spawn(fun loop/0)),

    Dispatch = cowboy_router:compile([
        {'_', [{"/download", slave_handler, #{secret_key => list_to_binary(SecretKey)}}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{
        env => #{dispatch => Dispatch}}),

    slave_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
start_nodes(false, _NumReplicas, _PrivateKey) ->
    [];

start_nodes(true, NumReplicas, _PrivateKey) ->
    add_node(NumReplicas,[], _PrivateKey).


add_node(NumReplicas, Nodes, _PrivateKey) when NumReplicas > 0 -> 
    Port = 5000 + NumReplicas,
    NameStr = "slave" ++ integer_to_list(NumReplicas),
    Name = list_to_atom(NameStr),
    Args = ["-setcookie", "system", "-env", "PORT", integer_to_list(Port), "-env", "SECRET_KEY", _PrivateKey],
    {ok, _, Node} = ?CT_PEER(#{host => "127.0.0.1", name => Name, args => Args}),
    % elp:ignore W0014 (cross_node_eval)
    rpc:call(Node, application, set_env, [ws, bootstrap, false]),
    lists:foreach(
        fun(Path) ->
            % elp:ignore W0014 (cross_node_eval)
            rpc:call(Node, code, add_pathz, [Path])
        end,
        code:get_path()
    ),
    io:format("[INFO] Started node ~p~n", [Node]),
    % elp:ignore W0014 (cross_node_eval)
    Res = rpc:call(Node, application, ensure_all_started, [slave]),
    io:format("[INFO] Response: ~p~n", [Res]),
    add_node(NumReplicas - 1, Nodes++[Node], _PrivateKey);

add_node(0, Nodes, _) ->
    Nodes.

loop() ->
    receive
        % receive a message from the master with the file name and content
        {file, FileName, FileContent} ->
            io:format("[INFO] Received file: ~p~n", [FileName]),
            save_file:save_file(FileName, FileContent, node()),
            loop();

        Other ->
            io:format("[INFO] Unknown messagge: ~p~n", [Other]),
            loop()
    end.

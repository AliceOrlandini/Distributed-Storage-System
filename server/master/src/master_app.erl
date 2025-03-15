%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = case os:getenv("PORT") of
        false ->
            8081;
        PortStr ->
            io:format("PortStr: ~p~n", [PortStr]), 
           {PortInt, _} = string:to_integer(PortStr),
            PortInt
    end,
    SecretKey = case os:getenv("SECRET_KEY") of
        false ->
            "";
        SecretKeyStr ->
            SecretKeyStr
    end,
    io:format("Starting master app on port ~p~n", [Port]),  
    NumReplicas = case application:get_env(ws, num_replicas) of
        {ok, N} -> N;
        undefined -> 0
    end,
    io:format("Number of replicas: ~p~n", [NumReplicas]),

    IsBootstrap = case application:get_env(ws, bootstrap) of
        {ok, B} -> B;
        undefined -> false
    end,
    Nodes = start_nodes(IsBootstrap, NumReplicas, SecretKey),

    lists:foreach(fun(Node) ->
        io:format("Ping ~p: ~p~n", [Node, net_adm:ping(Node)])
    end, Nodes),
    init_db(IsBootstrap, Nodes++[node()]),

    Dispatch = cowboy_router:compile([
        {'_', [{"/upload", master_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{env => #{dispatch => Dispatch}}),
    master_sup:start_link().
stop(_State) ->
    ok.


start_nodes(false, _NumReplicas, _PrivateKey) ->
    [];
start_nodes(true, NumReplicas, PrivateKey) ->
    add_node(NumReplicas,[], PrivateKey).
add_node(NumReplicas,Nodes,PrivateKey) when NumReplicas > 0 -> 
    Port = 8080 + NumReplicas,
    NameStr = "master" ++ integer_to_list(NumReplicas),
    Name = list_to_atom(NameStr),
    Args = "-setcookie system -env PORT " ++ integer_to_list(Port) ++ " -env SECRET_KEY " ++ PrivateKey,
    {ok, Node} = slave:start("127.0.0.1", Name, Args),
    rpc:call(Node, application, set_env, [ws, bootstrap, false]),
    {_, Slaves} = application:get_env(ws, slave_nodes),
    rpc:call(Node, application, set_env, [ws, slave_nodes, Slaves]),
    lists:foreach(
        fun(Path) ->
                rpc:call(Node, code, add_pathz, [Path])
        end,
        code:get_path()
    ),
    rpc:call(Node, application, ensure_all_started, [master]),
    add_node(NumReplicas-1,Nodes++[Node], PrivateKey);
add_node(0, Nodes, _) ->
    Nodes.
    
%% internal functions
init_db(true, Nodes) ->
    mnesia:create_schema(Nodes),
    {_Results, BadNodes} = rpc:multicall(Nodes, application, ensure_all_started, [mnesia]),
    case BadNodes of
        [] ->
            mnesia:start();
        _ ->
            io:format("Errore nell'avvio di mnesia su alcuni nodi: ~p~n", [BadNodes]),
            exit({mnesia_start_failed, BadNodes})
    end,
    io:format("Mnesia configurato su ~p res check ~p~n", [Nodes, _Results]),
    mnesia:start(),
    master_db:create_tables(Nodes);
init_db(false,_) ->
    ok.

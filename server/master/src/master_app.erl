%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).
-include_lib("common_test/include/ct.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = case os:getenv("PORT") of
        false ->
            3000;
        PortStr ->
            io:format("[INFO] Port: ~p~n", [PortStr]),
            {PortInt, _} = string:to_integer(PortStr),
            PortInt
    end,    

    SecretKey = case os:getenv("SECRET_KEY") of
        false ->
            "";
        SecretKeyStr ->
            SecretKeyStr
    end,

    ReplicaHosts = case application:get_env('Distributed-Storage-System', replica_hosts) of
        {ok, N} -> N;
        undefined -> 0
    end,

    IsBootstrap = case application:get_env('Distributed-Storage-System', bootstrap) of
        {ok, B} -> B;
        undefined -> false
    end,
    Nodes = start_nodes(IsBootstrap, ReplicaHosts, SecretKey, Port),

    lists:foreach(fun(Node) ->
        io:format("[INFO] Ping ~p: ~p~n", [Node, net_adm:ping(Node)])
    end, Nodes),
    init_db(IsBootstrap, Nodes ++ [node()]),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/login", master_login_handler, #{secret_key => list_to_binary(SecretKey)}},
            {"/registration", master_registration_handler, []},
            {"/upload", master_upload_handler, []},
            {"/files", master_get_files_handler, []},
            {"/fileparts", master_file_parts_handler, [#{secret_key => list_to_binary(SecretKey)}]},
            {"/share", master_share_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{
        env => #{dispatch => Dispatch, secret_key => list_to_binary(SecretKey)},
        middlewares => [master_middleware, cowboy_router, cowboy_handler]
    }),

    master_sup:start_link().

stop(_State) ->
    ok.


start_nodes(false, _HostReplicas, _PrivateKey, _Port) ->
    [];
start_nodes(true, HostReplicas, PrivateKey, Port) ->
    add_node(HostReplicas, 1, [], PrivateKey, Port).


add_node([Head | Tail], NumReplicas, Nodes, PrivateKey, Port) ->
    NameStr = "master" ++ integer_to_list(NumReplicas),
    Name = list_to_atom(NameStr),
    NameFull = lists:concat([atom_to_list(Name), "@", Head]),

    Ssh = os:find_executable("ssh"),

    Args = #{
        host => Head,
        name => Name,
        exec => {Ssh, ["-i", "/root/.ssh/test_ak", Head, "erl"]},
        args => ["-name", NameFull, "-setcookie", "system", 
                "-env", "PORT", integer_to_list(Port),
                "-env", "SECRET_KEY", PrivateKey]
    },
    {ok, _, Node} = ?CT_PEER(Args),
    rpc:call(Node, application, set_env, [ws, bootstrap, false]),
    lists:foreach(
        fun(Path) ->
            rpc:call(Node, code, add_pathz, [Path])
        end,
        code:get_path()
    ),

    rpc:call(Node, application, ensure_all_started, [master]),

    add_node(Tail, NumReplicas + 1, Nodes++[Node], PrivateKey, Port);
add_node([], _, Nodes, _, _) ->
    Nodes.

init_db(true, Nodes) ->
    mnesia:create_schema(Nodes),
    {Results, BadNodes} = rpc:multicall(Nodes, application, ensure_all_started, [mnesia]),
    case BadNodes of
        [] ->
            mnesia:start();
        _ ->
            io:format("[INFO] Mnesia start failed on nodes: ~p~n", [BadNodes]),
            exit({mnesia_start_failed, BadNodes})
    end,
    io:format("[INFO] Mnesia started on nodes: ~p. With result: ~p~n", [Nodes, Results]),
    mnesia:start(),
    master_db:create_tables(Nodes);
init_db(false, _) ->
    ok.

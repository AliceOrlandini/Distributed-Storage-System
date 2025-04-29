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

    %% Set config for HTTP server worker
    application:set_env(master, port, Port),
    application:set_env(master, secret_key, SecretKey),

    IsBootstrap = case os:getenv("BOOTSTRAP") of
        "true" -> true;
        "1" -> true;
        _ -> false
    end,
    NodesStr = case application:get_env('Distributed-Storage-System', replica_nodes) of
        {ok, N} when is_list(N) -> N;
        _ -> [node()]
        end,
    Nodes = [case N of
                S when is_list(S) -> list_to_atom(S);
                A when is_atom(A) -> A
             end || N <- NodesStr],


    case IsBootstrap of
        true -> 
            master_db:init_db_bootstrap(Nodes),
            start_nodes(Nodes, master_sup, start_http_server, []),
            master_sup:start_link(),
            master_sup:start_http_server();
        false ->
            master_sup:start_link()
    end.


start_nodes([Head | Tail], Module, Function, Args) ->
    io:format("[DEBUG] Starting node: ~p~n", [Head]),
    Result = rpc:call(Head, Module, Function, Args),
    case Result of
        {ok, _} -> ok;
        ok -> ok;
        Error -> io:format("[ERROR] rpc:call failed: ~p~n", [Error]), Error
    end,
    start_nodes(Tail, Module, Function, Args);
start_nodes([], _, _, _) ->
    ok.

stop(_State) ->
    ok.
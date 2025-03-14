%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case os:getenv("PORT") of
        PortStr ->
            {PortInt, _} = string:to_integer(PortStr),
            Port = PortInt;
        undefined ->
            {_Status, Port} = application:get_env(ws, port)
    end,
    io:format("Starting master app on port ~p~n", [Port]),
    {_, _Nodes} = application:get_env(ws, nodes),
    io:format("Nodes: ~p~n", [_Nodes]),
    {_, IsBoostrap} = application:get_env(ws, bootstrap),
    init_db(IsBoostrap),
    Dispatch = cowboy_router:compile([
        {'_', [{"/upload", master_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{env => #{dispatch => Dispatch}}),
    master_sup:start_link().
stop(_State) ->
    ok.

connect_to_node(Node) ->
    master_sup:connect_to_node(Node).


%% internal functions
init_db(true) ->
    io:format("init db");
init_db(false) ->
    ok.
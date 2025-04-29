%%%-------------------------------------------------------------------
%% @doc HTTP server worker for master node (OTP-compliant)
%%%-------------------------------------------------------------------

-module(master_http_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Read config from application env
    {ok, Port} = application:get_env(master, port),
    {ok, SecretKey} = application:get_env(master, secret_key),
    mnesia:start(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/login", master_login_handler, #{secret_key => list_to_binary(SecretKey)}},
            {"/registration", master_registration_handler, []},
            {"/upload", master_upload_handler, []},
            {"/files", master_get_files_handler, []},
            {"/fileparts", master_file_parts_handler, [#{secret_key => list_to_binary(SecretKey)}]},
            {"/share", master_share_handler, []},
            {"/delete", master_delete_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [
        {port, Port}
    ], #{
        env => #{dispatch => Dispatch, secret_key => list_to_binary(SecretKey)},
        middlewares => [master_middleware, cowboy_router, cowboy_handler]
    }),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

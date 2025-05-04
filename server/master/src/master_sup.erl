%%%-------------------------------------------------------------------
%% @doc master top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(master_sup).

-behaviour(supervisor).

-export([start_link/0, start_http_server/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

start_http_server() ->
    case whereis(master_http_server) of
        undefined ->
            supervisor:start_child(?SERVER, #{
                id => master_http_server,
                start => {master_http_server, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [master_http_server]
            });
        _Pid ->
            {ok, already_started}
    end.

%% internal functions

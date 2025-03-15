%%%-------------------------------------------------------------------
%% @doc slave public API
%% @end
%%%-------------------------------------------------------------------

-module(slave_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    slave_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

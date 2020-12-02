%%%-------------------------------------------------------------------
%% @doc scheduler_utilization public API
%% @end
%%%-------------------------------------------------------------------

-module(scheduler_utilization_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    scheduler_utilization_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc scheduler_utilization top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scheduler_utilization_sup).

-behaviour(supervisor).

-export([start_link/0]).

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
    ChildSpecs = [
        #{
            id => scheduler_utilization,
            start => {scheduler_utilization, start_link, []},
            restrt => permanent,
            shutdown => 10000,
            type => worker,
            modules => [scheduler_utilization]
        }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
-module(scheduler_utilization_SUITE).

-export([suite/0, init_per_suite/1, end_per_suite/1, groups/0, all/0]).
-export([total/1, weighted/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% Common Test API
-spec suite() -> term().
suite() ->
  [{timetrap, {seconds, 60}}].

-spec init_per_suite(term()) -> term().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(term()) -> term().
end_per_suite(_Config) ->
  ok.

-spec groups() -> term().
groups() ->
  [].

-spec all() -> term().
all() ->
  [total, weighted].

% Test suites
-spec total(term()) -> ok.
total(_Config) ->
  {ok, Pid} = scheduler_utilization:start_link([]),
  Sample1 = scheduler:sample(),
  Pid ! {update_utilization, Sample1},
  ?assert(scheduler_utilization:weighted() >= 0),
  ok.

-spec weighted(term()) -> ok.
weighted(_Config) ->
  {ok, Pid} = scheduler_utilization:start_link([]),
  Sample1 = scheduler:sample(),
  Pid ! {update_utilization, Sample1},
  ?assert(scheduler_utilization:weighted() >= 0),
  ok.


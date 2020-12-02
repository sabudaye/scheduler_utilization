-module(scheduler_utilization).

-behaviour(gen_server).

%% API
-export([total/0, weighted/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INTERVAL, 1000).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec total() -> number().
total() -> get_value(total, 0.0).

-spec weighted() -> number().
weighted() -> get_value(weighted, 0.0).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	ets:new(?MODULE, [
		named_table,
		{read_concurrency, true}
	]),
    Sample1 = scheduler:sample(),
    update_utilization(Sample1, Sample1),
    erlang:send_after(?INTERVAL, self(), {update_utilization, Sample1}),
	{ok, {}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({update_utilization, Sample1}, State) ->
    Sample2 = scheduler:sample(),
    update_utilization(Sample1, Sample2),
    erlang:send_after(?INTERVAL, self(), {update_utilization, Sample2}),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% internal functions
%%%===================================================================

get_value(Key, Default) ->
    case ets:info(?MODULE) of
        undefined -> Default;
        _ ->
            [Utilization] = ets:lookup(?MODULE, utilization),
            case lists:keyfind(Key, 1, Utilization) of
                false -> Default;
                Value -> Value
            end
    end.

update_utilization(Sample1, Sample2) ->
    Utilization = scheduler:utilization(Sample1, Sample2),
    ets:insert(?MODULE, {utilization, Utilization}).

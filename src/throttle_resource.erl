%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is an individula counter process, 
%% this proccess will be the child of a resource supervisor.
-module(throttle_resource).

%% Includes
-behaviour(gen_server).

%% Exports
-export([start_link/2, check/2, peek/2, restore/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

%% Module records.
-record(counter, {limit :: integer(), 
		  timeout :: integer(), 
		  die :: integer()}).
-record(state, {id :: atom(), 
		counterInit = #counter{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Create the resource gen_server process, the function receive the Id
%% of the resource receive an Id and the parameters to create throttle_counter. 
-spec start_link(Id :: atom(),
		 CounterInit :: {integer(), integer(), integer()}) -> pid().
start_link(Id, CounterInit) ->
    gen_server:start_link({local, Id}, ?MODULE, {Id, CounterInit}, []).


%% @doc Update the counter of the CounterId and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec check(atom(), atom()) -> {ok, integer()} | {error, integer()}.
check(ResourceId, CounterId) ->
    gen_server:call(ResourceId, {update_counter, CounterId}).


%% @doc Get the counter of the CounterId and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec peek(atom(), atom()) -> {ok, integer()} | {error, integer()}.
peek(ResourceId, CounterId) ->
    gen_server:call(ResourceId, {get_counter, CounterId}).


%% @doc Restore the counter of the CounterId and return a pair {ok, count}.
-spec restore(atom(), atom()) -> {ok, integer()}.
restore(ResourceId, CounterId) ->
    gen_server:call(ResourceId, {restore_counter, CounterId}).


%% @doc Stop the counter gen_server process independently of the state.
-spec stop(atom()) -> ok.
stop(Id) -> 
    gen_server:call(Id, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server functions

%% @doc Constructor of the resource gen_server process.
init({Id, {LimitCounter, TimeoutCounter, DieCounter}}) ->
    %% process_flag(trap_exit, false),
    CounterInit = #counter{limit = LimitCounter,
			   timeout = TimeoutCounter,
			   die = DieCounter},
    {ok, #state{id = Id, counterInit = CounterInit}}.


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call({update_counter, CounterId}, _From, State) ->
    Init = State#state.counterInit,
    {_, Pid} = throttle_counter:start_link(CounterId, Init#counter.limit, Init#counter.timeout, Init#counter.die),
    Reply = throttle_counter:check(CounterId),
    {reply, Reply, State};


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call({get_counter, CounterId}, _From, State) ->
    Init = State#state.counterInit,
    throttle_counter:start_link(CounterId, Init#counter.limit, Init#counter.timeout, Init#counter.die),
    Reply = throttle_counter:peek(CounterId),
    {reply, Reply, State};


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_call({restore_counter, CounterId}, _From, State) ->
    Init = State#state.counterInit,
    throttle_counter:start_link(CounterId, Init#counter.limit, Init#counter.timeout, Init#counter.die),
    Reply = throttle_counter:restore(CounterId),
    {reply, Reply, State};


%% @doc Stop the counter gen_server process independently of the state.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


%% @doc Process the undefine call.
handle_call(Command, _, State) ->
    Reply = {error, "The " ++ atom_to_list(Command) ++ " is undefine"},
    {reply, Reply, State}.


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_info({restore_counter, CounterId}, State) ->
    Init = State#state.counterInit,
    throttle_counter:start_link(CounterId, Init#counter.limit, Init#counter.timeout, Init#counter.die),
    throttle_counter:restore(CounterId),
    {noreply, State};


%% @doc Process the undefine call.
handle_info(_Message, State) ->
    {noreply, State}.


%% @doc Get the state of the counter
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Stop the counter independently the type.
terminate(_, State) ->
    {stop, inactivity, State}.


%% @doc Process the undefine call.
handle_cast(_Message, State) ->
    {noreply, State}.

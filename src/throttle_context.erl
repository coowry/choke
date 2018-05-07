%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is an individula counter process, 
%% this proccess will be the child of a context supervisor.
-module(throttle_context).

%% Includes
-behaviour(gen_server).

%% Exports
-export([start_link/2, check/2, peek/2, restore/2, stop/1, kick/3]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

%% Module records.
-record(counter, {limit :: integer(), 
		  timeout :: integer()}).
-record(state, {id :: atom(),
		child :: map(),
		counterInit = #counter{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Create the context gen_server process, the function receive the Id
%% of the context receive an Id and the parameters to create throttle_counter. 
-spec start_link(Id :: atom(),
		 CounterInit :: {integer(), integer()}) -> pid().
start_link(Id, CounterInit) ->
    gen_server:start_link({local, Id}, ?MODULE, {Id, CounterInit}, []).


%% @doc Update the counter of the CounterId and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec check(atom(), atom()) -> {ok, integer()} | {error, integer()}.
check(ContextId, CounterId) ->
    gen_server:call(ContextId, {update_counter, CounterId}).


%% @doc Get the counter of the CounterId and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec peek(atom(), atom()) -> {ok, integer()} | {error, integer()}.
peek(ContextId, CounterId) ->
    gen_server:call(ContextId, {get_counter, CounterId}).


%% @doc Restore the counter of the CounterId and return a pair {ok, count}.
-spec restore(atom(), atom()) -> {ok, integer()}.
restore(ContextId, CounterId) ->
    gen_server:call(ContextId, {restore_counter, CounterId}).


%% @doc Stop the counter gen_server process independently of the state.
-spec stop(atom()) -> ok.
stop(Id) -> 
    gen_server:call(Id, stop).


%% @doc
-spec kick(pid(), atom(), pid()) -> ok.
kick(Pid, Id, CounterPid) ->
    gen_server:cast(Pid, {delete_counter, Id, CounterPid}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server functions

%% @doc Constructor of the context gen_server process.
init({Id, {LimitCounter, TimeoutCounter}}) ->
    process_flag(trap_exit, false),
    CounterInit = #counter{limit = LimitCounter,
			   timeout = TimeoutCounter},
    {ok, #state{id = Id, counterInit = CounterInit, child = #{}}}.


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call({update_counter, CounterId}, _From, State) ->
    {NewState, Pid} = get_child(State, CounterId),
    Reply = throttle_counter:check(Pid),
    {reply, Reply, NewState};


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call({get_counter, CounterId}, _From, State) ->
    {NewState, Pid} = get_child(State, CounterId),
    Reply = throttle_counter:peek(Pid),
    {reply, Reply, NewState};


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_call({restore_counter, CounterId}, _From, State) ->
    {NewState, Pid} = get_child(State, CounterId),
    Reply = throttle_counter:restore(Pid),
    {reply, Reply, NewState};


%% @doc Stop the counter gen_server process independently of the state.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


%% @doc Process the undefine call.
handle_call(Command, _, State) ->
    Reply = {error, "The " ++ atom_to_list(Command) ++ " is undefine"},
    {reply, Reply, State}.


%% @doc Process the undefine call.
handle_info(_Message, State) ->
    {noreply, State}.


%% @doc Get the state of the counter
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Stop the counter independently the type.
terminate(_, _State) ->
    ok.


%% @doc
handle_cast({delete_counter, CounterId, CounterPid}, State) ->
    Map = State#state.child,
    unlink(CounterPid),
    {noreply, State#state{child = maps:without([CounterId], Map)}};


%% @doc Process the undefine call.
handle_cast(_Message, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
get_child(State, ChildId) ->
    Map = State#state.child,
    GetPid = case maps:is_key(ChildId, Map) of
		 false ->
		     Init = State#state.counterInit,
		     {ok, Pid} = throttle_counter:start_link(ChildId, self(), Init#counter.limit, Init#counter.timeout, 3 * Init#counter.timeout),
		     Pid;
		 _ -> Pid = maps:get(ChildId, Map), 
		      case process_info(Pid) of
			  undefined -> Init = State#state.counterInit,
				       {ok, Pid1} = throttle_counter:start_link(ChildId, self(), Init#counter.limit, Init#counter.timeout, 3 * Init#counter.timeout),
				       Pid1;
			  _ -> Pid
		      end
	     end,
    {State#state{child = Map#{ChildId => GetPid}}, GetPid}.
	    

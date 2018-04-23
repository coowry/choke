%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is an individula counter process, 
%% this proccess will be the child of a resource supervisor.
-module(throttle_counter).

%% Includes
-behaviour(gen_server).

%% Exports
-export([start_link/4, start_link/3, check/1, peek/1, 
	 restore/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

%% Module records.
-record(state, {limit :: integer(), 
		count :: integer(), 
		timeout :: integer(), 
		die :: integer()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Create the counter gen_server process, the function receive the Id
%% of the counter, the Limit of times you can call the check proces in a 
%% time definite by the Timeout.
-spec start_link(Id :: atom(), Limit :: integer(), 
		 Timeout :: atom() | integer(), Die :: integer) -> pid().
start_link(Id, Limit, Timeout, Die) ->
    gen_server:start_link({local, Id}, ?MODULE, {Limit, Timeout, Die}, []).


%% @doc Create the counter gen_server process, the function receive the Id
%% of the counter, the Limit of times you can call the check proces in a 
%% time definite by the Timeout.
-spec start_link(Limit :: integer(), Timeout :: atom() | integer(), 
		 Die :: integer) -> pid().
start_link(Limit, Timeout, Die) ->
    gen_server:start_link(?MODULE, {Limit, Timeout, Die}, []).


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec check(atom() | pid()) -> {ok, integer()} | {error, integer()}.
check(Id) ->
    gen_server:call(Id, update_counter).


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit.
-spec peek(atom() | pid()) -> {ok, integer()} | {error, integer()}.
peek(Id) ->
    gen_server:call(Id, get_counter).


%% @doc Restore the internal counter and return a pair {ok, count}.
-spec restore(atom() | pid()) -> {ok, integer()}.
restore(Id) ->
    gen_server:call(Id, restore_counter).


%% @doc Stop the counter gen_server process independently of the state.
-spec stop(atom() | pid()) -> ok.
stop(Id) -> 
    gen_server:call(Id, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server functions

%% @doc Constructor of the counter gen_server process.
init({Limit, Timeout, Die}) ->
    %% process_flag(trap_exit, true),
    {ok, #state{limit = Limit, count = 0, timeout = Timeout, die = Die}, Die}.


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call(update_counter, _From, State) ->
    Limit = State#state.limit,
    Count = State#state.count,
    Die = State#state.die,
    if Limit > Count ->
	    if Count == 0 ->
		    erlang:send_after(State#state.timeout, self(), restore_counter);
	       true -> ok
	    end,    
    	    UpdateCount = Count + 1,
    	    NewState = State#state{count = UpdateCount},
	    {reply, {ok, UpdateCount}, NewState, Die};
       true -> {reply, {error, State#state.timeout}, State, Die}
    end;


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call(get_counter, _From, State) ->
    Limit = State#state.limit,
    Count = State#state.count,
    Die = State#state.die,
    if Limit > Count -> {reply, {ok, Count}, State, Die};
       true -> {reply, {error, wait}, State, Die}
    end;


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_call(restore_counter, _From, State) ->
    {reply, {ok, 0},State#state{count = 0}, State#state.die};


%% @doc Stop the counter gen_server process independently of the state.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


%% @doc Process the undefine call.
handle_call(Command, _, State) ->
    Reply = {error, "The " ++ atom_to_list(Command) ++ " is undefine"},
    {reply, Reply, State, State#state.die}.


%% @doc Process the timeout request.
handle_info(timeout, State) ->
    {stop, normal, State};


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_info(restore_counter, State) ->
    {noreply, State#state{count = 0}, State#state.die};


%% @doc Process the undefine call.
handle_info(_Message, State) ->
    {noreply, State, State#state.die}.


%% @doc Get the state of the counter
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Stop the counter independently the type.
terminate(_, State) ->
    {stop, inactivity, State}.


%% @doc Process the undefine call.
handle_cast(_Message, State) ->
    {noreply, State, State#state.die}.

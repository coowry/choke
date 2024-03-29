%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is an individula counter process, 
%% this proccess will be the child of a context supervisor.
-module(throttle_counter).

%% Includes
-behaviour(gen_server).

%% Exports
-export([start_link/5, start_link_name/5, check/1, check/2, 
         peek/1, restore/1, stop/1, get/1]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

%% Module records.
-record(state, {id :: atom(),
		parent :: pid(),
		limit :: integer(), 
		count :: integer(), 
		timeout :: integer(),
		die :: integer(),
                blocked :: boolean()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Create the counter gen_server process, the function receive the Id
%% of the counter, the Limit of times you can call the check proces in a 
%% time definite by the Timeout.
-spec start_link_name(Id :: atom(), Parent :: pid(), Limit :: integer(), 
                      Timeout :: atom() | integer(), Die :: integer()) ->  
                         {ok, pid()} | ignore | {error, term()}.
start_link_name(Id, Parent, Limit, Timeout, Die) ->
  gen_server:start_link({local, Id}, ?MODULE, {Id, Parent, Limit, Timeout, Die}, []).


%% @doc Create the counter gen_server process, the function receive the Id
%% of the counter, the Limit of times you can call the check proces in a 
%% time definite by the Timeout.
-spec start_link(Id :: atom(), Parent :: pid(), Limit :: integer(), 
		 Timeout :: atom() | integer(), Die :: integer()) ->  
                    {ok, pid()} | ignore | {error, term()}.
start_link(Id, Parent, Limit, Timeout, Die) ->
  gen_server:start_link(?MODULE, {Id, Parent, Limit, Timeout, Die}, []).


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error | warning, counter, timeout} if you exceed the limit.
-spec check(atom() | pid()) -> {ok, integer()} | {warning | error, integer(), integer()}.
check(Id) ->
  check(Id, [{strict, false}]).


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error | warning, counter, timeout} if you exceed the limit.
%% Also include a option parameter.
-spec check(atom() | pid(), [{atom(), boolean()}]) -> {ok, integer()} | {warning | error, integer(), integer()}.
check(Id, [{strict, true}]) ->
  gen_server:call(Id, update_counter_strict);

check(Id, [{strict, false}]) ->
  gen_server:call(Id, update_counter).


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error | warning, counter, timeout} if you exceed the limit.
-spec peek(atom() | pid()) -> {ok, integer()} | {warning | error, integer(), integer()}.
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


%% @doc Get information about a Counter
-spec get(atom() | pid()) -> #{id => atom(),
                               count => integer(), 
                               blocked => boolean()}.
get(Id) ->
  gen_server:call(Id, get).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server functions

%% @doc Constructor of the counter gen_server process.
init({Id, Parent, Limit, Timeout, Die}) ->
  process_flag(trap_exit, true),
  {ok, #state{id = Id, parent = Parent, limit = Limit, count = 0, 
              timeout = Timeout, die = Die, blocked = false}, Die}.


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call(update_counter, _From, State) ->
  Limit = State#state.limit,
  Count = State#state.count,
  Die = State#state.die,
  Blocked = State#state.blocked,
  if Limit > Count ->
      erlang:send_after(State#state.timeout, self(), sub_counter),
      UpdateCount = Count + 1,
      NewState = if Blocked -> 
                     State#state{count = UpdateCount, blocked = false};
                    true ->
                     State#state{count = UpdateCount}
                 end,
      {reply, {ok, UpdateCount}, NewState, Die};
     not Blocked ->
      NewState = State#state{blocked = true},
      {reply, {warning, Limit, State#state.timeout}, NewState, Die};
     true -> 
      {reply, {error, Limit, State#state.timeout}, State, Die}
  end;


%% @doc Update the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call(update_counter_strict, _From, State) ->
  Limit = State#state.limit,
  Count = State#state.count,
  Die = State#state.die,
  Blocked = State#state.blocked,
  %% Update the counter
  erlang:send_after(State#state.timeout, self(), sub_counter),
  UpdateCount = Count + 1,
  if Limit > Count ->
      NewState = if Blocked -> 
                     State#state{count = UpdateCount, blocked = false};
                    true ->
                     State#state{count = UpdateCount}
                 end,
      {reply, {ok, UpdateCount}, NewState, Die};
     not Blocked ->
      NewState = State#state{blocked = true, count = UpdateCount},
      {reply, {warning, Limit, State#state.timeout}, NewState, Die};
     true -> 
      NewState = State#state{count = UpdateCount},
      {reply, {error, Limit, State#state.timeout}, NewState, Die}
  end;


%% @doc Get the internal counter and return a pair {ok, count} if you are
%% between the limit or {error, timeout} if you exceed the limit. Also at the
%% end set the die timeout of the process if not receive any call.
handle_call(get_counter, _From, State) ->
  Limit = State#state.limit,
  Count = State#state.count,
  Die = State#state.die,
  Blocked = State#state.blocked,
  if Limit > Count ->
      {reply, {ok, Count + 1}, State, Die};
     not Blocked ->
      {reply, {warning, Limit, State#state.timeout}, State, Die};
     true -> 
      {reply, {error, Limit, State#state.timeout}, State, Die}
  end;


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_call(restore_counter, _From, State) ->
  {reply, {ok, 0}, State#state{count = 0}, State#state.die};



%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_call(get, _From, State) ->
  Data = #{id => State#state.id,
           count => State#state.count, 
           blocked => State#state.blocked},
  {reply, Data, State, State#state.die};


%% @doc Stop the counter gen_server process independently of the state.
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};


%% @doc Process the undefine call.
handle_call(Command, _, State) ->
  Reply = {error, "The " ++ atom_to_list(Command) ++ " is unddefine"},
  {reply, Reply, State, State#state.die}.


%% @doc Process the timeout request.
handle_info(timeout, State) ->
  {stop, normal, State};


%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_info(restore_counter, State) ->
  {noreply, State#state{count = 0}, State#state.die};



%% @doc Restore the internal counter also at the end set the die timeout of
%% the process if not receive any call.
handle_info(sub_counter, State) ->
  Count = State#state.count,      
  {noreply, State#state{count = Count - 1}, State#state.die};


%% @doc Process the undefine call.
handle_info(_Message, State) ->
  {noreply, State, State#state.die}.


%% @doc Get the state of the counter
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @doc Stop the counter independently the type.
terminate(_, State) ->
  Id = State#state.id,
  Parent = State#state.parent,
  throttle_context:kick(Parent, Id, self()),
  ok.


%% @doc Process the undefine call.
handle_cast(_Message, State) ->
  {noreply, State, State#state.die}.

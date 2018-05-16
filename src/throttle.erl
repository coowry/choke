%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is a supervisor for the context gen_servers.
%% This module has the functions to interact with the other modules.
%% IMPORTANT: You are only able to star one throttle supervisor.
-module(throttle).

%% Includes
-behaviour(supervisor).

%% Exports
-export([start_link/0, start_context/2, check/2, 
	 stop/1, stop/0, peek/2, restore/2, restart/1, 
	 init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions

%% @doc Start the supervisor.
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    process_flag(trap_exit, true),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Create the context gen_server process, the function receive the Id
%% of the context and the parameters to create throttle_counter. 
-spec start_context(Id :: atom(),
		     CounterInit :: {integer(), integer()}) -> {ok, pid()}.
start_context(Id, {Limit, Timeout}) ->
    ChildSpec = #{id => Id,
		  start => {throttle_context, start_link, [Id, {Limit, Timeout}]},
		  restart => permanent,
		  shutdown => 10500,
		  type => worker,
		  modules => [throttle_context]
     },
    supervisor:start_child(?MODULE, ChildSpec).


%% @doc Update the counter of the CounterId belonging to the context 
%% ContextId, and return a pair {ok, count} if you are between the limit or 
%% {error, limit, timeout} if you exceed the limit.
-spec check(atom(), atom()) -> {ok, integer()} | {error, integer()}.
check(ContextId, CounterId) ->
    throttle_context:check(ContextId, CounterId).


%% @doc Get the counter of the CounterId belonging to the context 
%% ContextId and return a pair {ok, count} if you are between the limit or 
%% {error, timeout} if you exceed the limit.
-spec peek(atom(), atom()) -> {ok, integer()} | {error, integer()}.
peek(ContextId, CounterId) ->
    gen_server:call(ContextId, {get_counter, CounterId}).


%% @doc Restore the counter of the CounterId belonging to the context 
%% ContextId and return a pair {ok, count}.
-spec restore(atom(), atom()) -> {ok, integer()}.
restore(ContextId, CounterId) ->
    gen_server:call(ContextId, {restore_counter, CounterId}).


%% @doc Restart the context gen_server process independently of the state.
-spec restart(atom()) -> ok.
restart(ContextId) ->
    case whereis(ContextId) of
	undefined -> undefined;
	A -> 
	    unlink(A),
	    throttle_context:stop(ContextId)
    end.


%% @doc Stop the context gen_server process independently of the state.
-spec stop(atom() | pid()) -> ok | {error, atom() | pid()}.
stop(ContextId) when is_atom(ContextId) ->
    supervisor:terminate_child(?MODULE, ContextId),
    supervisor:delete_child(?MODULE, ContextId);

stop(ContextId) when is_pid(ContextId) ->
    case process_info(ContextId, registered_name) of
	{registered_name, Name} ->
	    stop(Name);
	_ -> {error, ContextId}
    end.


%% @doc Stop the throttle supervisor process independently of the state.
-spec stop() -> true.
stop() ->
    exit(whereis(?MODULE), shutdown).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% supervisor functions

%% @doc Constructor of the throttle supervisor process.
init([]) ->
    MaxRestart = 6,
    MaxTime = 3000,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.



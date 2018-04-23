%% @author Coowry Developers Team
%%
%% @copyright 2018 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc This module is an individula counter process, 
%% this proccess will be the child of a resource supervisor.
-module(throttle).

%% Includes
-behaviour(supervisor).

%% Exports
-export([start_link/0, init/1, start_resource/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    MaxRestart = 6,
    MaxTime = 3000,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.


start_resource(Id, {Limit, Timeout, Die}) ->
    ChildSpec = #{id => Id,
		  start => {throttle_resource, start_link, [Id, {Limit, Timeout, Die}]},
		  restart => permanent,
		  shutdown => 10500,
		  type => worker,
		  modules => [throttle_resource]
     },
    supervisor:start_child(?MODULE, ChildSpec).

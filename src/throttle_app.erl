-module(throttle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for Coowry Core (cwc).
start(_Type, _StartArgs) ->
    {ok, Pid} = throttle:start_link(),
    case application:get_env(throttle, resources) of
	{ok, Resources} ->
	    lists:foreach(fun({Name, Init}) ->
				  throttle:start_resource(Name, Init)
			  end, Resources);
	_ ->
	    ok
    end,
    {ok, Pid}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for Coowry Core (cwc).
stop(_State) ->
    ok.

-module(throttle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for throttle.
start(_Type, _StartArgs) ->
    {ok, Pid} = throttle:start_link(),
    case application:get_env(throttle, resources) of
	{ok, Resources} ->
	    lists:foreach(fun({Id, {Limit, Timeout, Die}}) ->
				  throttle:start_resource(Id, {Limit, Timeout, Die})
			  end, Resources);
	_ ->
	    ok
    end,
    {ok, Pid}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for throttle.
stop(_State) ->
    ok.

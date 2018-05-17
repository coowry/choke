-module(throttle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for throttle.
start(_Type, _StartArgs) ->
    {ok, Pid} = throttle:start_link(),
    case application:get_env(throttle, resources) of
	{ok, Resources} ->
	    lists:foreach(fun({Id, {Limit, Timeout}}) ->
				  throttle:start_context(Id, {Limit, Timeout})
			  end, Resources);
	_ ->
	    ok
    end,
    {ok, Pid}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for throttle.
stop(_State) ->
    throttle:stop(),
    ok.

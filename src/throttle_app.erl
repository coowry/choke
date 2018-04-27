-module(throttle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for Coowry Core (cwc).
start(_Type, _StartArgs) ->
    throttle:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for Coowry Core (cwc).
stop(_State) ->
    ok.

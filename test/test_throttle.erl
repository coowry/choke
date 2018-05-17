%% @author Coowry developers team
%%
%% @copyright 2013, 2014 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Testing module counter

-module(test_throttle).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================
create_link_test_() ->
    ?_test(begin
	       ?debugFmt("Create Link", []),
	       {ok, Pid} = throttle:start_link(),
	       ?assertNotEqual('undefined', process_info(Pid)),
	       throttle:stop()
	   end).

create_contest_test_() ->
    [
     ?_test(begin
		?debugFmt("Create Link", []),
		throttle:start_link(),
		{ok, Pid} = throttle:start_context('context1', {2, 5000}),
		?assertNotEqual('undefined', process_info(Pid)),
	        throttle:stop('context1'),
		throttle:stop()
	   end),
     ?_test(begin
		?debugFmt("Create Multiple Link", []),
		throttle:start_link(),
		List = [
			throttle:start_context('context1', {2, 5000}),
			throttle:start_context('context2', {3, 5000}),
		        throttle:start_context('context3', {4, 6000}),
		        throttle:start_context('context4', {5, 7000}),
		        throttle:start_context('context5', {6, 100}),
		        throttle:start_context('context6', {7, 95000})
		       ],
		Check = fun({ok, Pid}) ->
				?assertNotEqual('undefined', process_info(Pid)),
				throttle:stop(Pid)
			end,
		lists:foreach(Check, List),
		throttle:stop()
	    end)
    ].

check_peek_test_() ->
    [
     ?_test(?debugFmt("Check and Peek Test", [])),
     ?_test(begin
		Context = 'context1',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context, {2, 1000}),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:peek(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context, Counter)),
	        throttle:stop(Context),
		throttle:stop()
	    end),
     ?_test(begin
		Context = 'context1',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context, {2, 1000}),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter)),
		timer:sleep(2000),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		throttle:stop(Context),
		throttle:stop()
	    end),
     ?_test(begin
     		Context = 'context1',
     		Counter1 = 'counter1',
     		Counter2 = 'counter2',
     		throttle:start_link(),
     		throttle:start_context(Context, {2, 1000}),
     		?assertEqual({ok, 1},  throttle:check(Context, Counter1)),
		?assertEqual({ok, 1},  throttle:check(Context, Counter2)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter1)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context, Counter1)),
		?assertEqual({ok, 2}, throttle:check(Context, Counter2)),
		throttle:stop(Context),
		throttle:stop()
     	    end),
     ?_test(begin
     		Context1 = 'context1',
		Context2 = 'context2',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context1, {2, 1000}),
		throttle:start_context(Context2, {3, 2000}),
		?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 1},  throttle:check(Context2, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context2, Counter)),
		?assertEqual({ok, 3},  throttle:check(Context2, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context1, Counter)),
		?assertEqual({error, 3, 2000}, throttle_context:check(Context2, Counter)),
		throttle:stop(Context1),
		throttle:stop(Context2),
		throttle:stop()
     	    end),
     ?_test(begin
		Context1 = 'context1',
		Context2 = 'context2',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context1, {2, 1000}),
		throttle:start_context(Context2, {3, 2000}),
		?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 1},  throttle:check(Context2, Counter)),
		timer:sleep(1000),
		?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context2, Counter)),
		timer:sleep(1000),
		?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context2, Counter)),
		throttle:stop(Context1),
		throttle:stop(Context2),
		throttle:stop()
	    end)
    ].

restore_test_() ->
    [
     ?_test(?debugFmt("Restore Test", [])),
     ?_test(begin
		Context = 'context1',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context, {2, 1000}),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter)),
		?assertEqual({ok, 0},  throttle:restore(Context, Counter)),
	        ?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		throttle:stop(Context),
		throttle:stop()
	    end),
     ?_test(begin
     		Context = 'context1',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context, {2, 1000}),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter)),
		?assertEqual({error, 2, 1000},  throttle:check(Context, Counter)),
		?assertEqual({ok, 0},  throttle:restore(Context, Counter)),
	        ?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		throttle:stop(Context),
		throttle:stop()
	    end),
     ?_test(begin
		Context = 'context1',
     		Counter1 = 'counter1',
     		Counter2 = 'counter2',
     		throttle:start_link(),
     		throttle:start_context(Context, {2, 1000}),
     		throttle:check(Context, Counter1),
		throttle:check(Context, Counter1),
		throttle:check(Context, Counter1),
		throttle:check(Context, Counter1),
		throttle:check(Context, Counter2),
     		?assertEqual({ok, 0},  throttle:restore(Context, Counter1)),
	        ?assertEqual({ok, 1},  throttle:check(Context, Counter1)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter2)),
		throttle:stop(Context),
		throttle:stop()
     	    end),
     ?_test(begin
		Context1 = 'context1',
		Context2 = 'context2',
		Counter = 'counter1',
		throttle:start_link(),
		throttle:start_context(Context1, {2, 1000}),
		throttle:start_context(Context2, {3, 2000}),
		?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
		?assertEqual({ok, 1},  throttle:check(Context2, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context1, Counter)),
		?assertEqual({error, 2, 1000},  throttle:check(Context1, Counter)),
     		?assertEqual({ok, 0},  throttle:restore(Context1, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context2, Counter)),
		throttle:stop(Context1),
		throttle:stop(Context2),
		throttle:stop()
	    end)
    ].

restart_test_() ->
    ?_test(?debugFmt("Restart Test", [])),
    ?_test(begin
	       Context = 'context1',
	       Counter1 = 'counter1',
	       Counter2 = 'counter2',
	       throttle:start_link(),
	       throttle:start_context(Context, {2, 1000}),
	       throttle:check(Context, Counter1),
	       throttle:check(Context, Counter2),
	       throttle:restart(Context),
	       timer:sleep(1000),
	       ?assertEqual({ok, 1},  throttle:check(Context, Counter1)),
	       ?assertEqual({ok, 1},  throttle:check(Context, Counter2)),
	       throttle:stop(Context),
	       throttle:stop()
	   end).

env_test_() ->
    ?_test(begin
	       application:set_env(throttle, resources, [
							 {'callbacks', {4, 5000}},
							 {'trades', {5, 1000}},
							 {'actions', {1000, 10000}},
							 {'authorization', {4, 5000}}
							]),
	       application:start(throttle),
	       ?assertMatch({error,{already_started,_}}, throttle:start_context('callbacks', {4, 5000})),
	       ?assertMatch({error,{already_started,_}}, throttle:start_context('trades', {5, 1000})),
	       ?assertMatch({error,{already_started,_}}, throttle:start_context('actions', {1000, 10000})),
	       ?assertMatch({error,{already_started,_}}, throttle:start_context('authorization', {4, 5000})),	       
	       application:stop(throttle)
	   end).

application_test_() ->
    ?_test(begin
	       ?debugFmt("Check and Peek Test", []),
	       application:start(throttle),
	       Context1 = 'context1',
	       Context2 = 'context2',
	       Counter = 'counter1',
	       throttle:start_context(Context1, {2, 1000}),
	       throttle:start_context(Context2, {3, 2000}),
	       ?assertEqual({ok, 1},  throttle:check(Context1, Counter)),
	       ?assertEqual({ok, 1},  throttle:check(Context2, Counter)),
	       ?assertEqual({ok, 2},  throttle:check(Context1, Counter)),
	       ?assertEqual({ok, 2},  throttle:check(Context2, Counter)),
	       ?assertEqual({ok, 3},  throttle:check(Context2, Counter)),
	       ?assertEqual({error, 2, 1000}, throttle_context:check(Context1, Counter)),
	       ?assertEqual({error, 3, 2000}, throttle_context:check(Context2, Counter)),
	       throttle:stop(Context1),
	       throttle:stop(Context2),
	       application:stop(throttle)
	   end).

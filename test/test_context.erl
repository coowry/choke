%% @author Coowry developers team
%%
%% @copyright 2013, 2014 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Testing module counter

-module(test_context).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================
create_link_test_() ->
    [
     ?_test(begin
		?debugFmt("Create Link", []),
		{ok, Pid} = throttle_context:start_link('context1', {2, 5000}),
		?assertNotEqual('undefined', process_info(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 5000}),
		?assertEqual(Pid, whereis('context1')),
		?assertNotEqual('undefined', process_info(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		?debugFmt("Create Multiple Link", []),
		List = [
			throttle_context:start_link('context1', {2, 5000}),
			throttle_context:start_link('context2', {3, 5000}),
			throttle_context:start_link('context3', {4, 6000}),
			throttle_context:start_link('context4', {5, 7000}),
			throttle_context:start_link('context5', {6, 100}),
			throttle_context:start_link('context6', {7, 95000})
		       ],
		Check = fun({ok, Pid}) ->
				?assertNotEqual('undefined', process_info(Pid)),
				unlink(Pid),
				exit(Pid, kill)
			end,
		lists:foreach(Check, List)
	    end)
    ].

stop_test_() ->
    ?_test(begin
	       ?debugFmt("Create Link", []),
	       {ok, Pid} = throttle_context:start_link('context1', {2, 5000}),
	       ?assertNotEqual('undefined', process_info(Pid)),
	       unlink(Pid),
	       throttle_context:stop(Pid)
	   end).


check_peek_test_() ->
    [
     ?_test(?debugFmt("Check and Peek Test", [])),
     ?_test(begin
		Name = 'counter1',
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		?assertEqual({ok, 2},  throttle_context:check(Pid, Name)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Pid, Name)),
		?assertEqual({error, 2, 1000}, throttle_context:peek(Pid, Name)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Pid, Name)),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end),
     ?_test(begin
		Name = 'counter1',
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		timer:sleep(2000),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter1')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter2')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter3')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter4')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter5')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter6')),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter1')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter2')),
		?assertEqual({ok, 2},  throttle_context:check(Pid, 'counter1')),
		?assertEqual({ok, 2},  throttle_context:check(Pid, 'counter2')),
		?assertEqual({error, 2, 1000},  throttle_context:check(Pid, 'counter1')),
		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter3')),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end)
    ].

restore_test_() ->
    [
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		Name = 'counter1',
	        throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
		?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:peek(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		Name = 'counter1',
	        throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
	        ?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:peek(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
		Name = 'counter1',
	        throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
		throttle_context:check(Pid, 'counter2'),
		throttle_context:check(Pid, 'counter2'),
		throttle_context:check(Pid, Name),
		throttle_context:check(Pid, Name),
	        ?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:peek(Pid, Name)),
		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
		?assertEqual({error, 2, 1000},  throttle_context:peek(Pid, 'counter2')),
		unlink(Pid),
		throttle_context:stop(Pid)
	    end)
    ].

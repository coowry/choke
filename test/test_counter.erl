%% @author Coowry developers team
%%
%% @copyright 2013, 2014 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc Testing module counter

-module(test_counter).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Tests
%% ===================================================================
create_link_test_() ->
    [
     ?_test(begin
		?debugFmt("Create Link", []),
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 2, 5000, 10000),
		?assertNotEqual('undefined', process_info(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		?debugFmt("Create Multiple Link", []),
		List = [
			throttle_counter:start_link('Counter1', self(), 4, 5000, 10000),
			throttle_counter:start_link('Counter2', self(), 5, 5000, 9000),
			throttle_counter:start_link('Counter3', self(), 6, 6000, 8000),
			throttle_counter:start_link('Counter4', self(), 7, 6000, 7000),
			throttle_counter:start_link('Counter5', self(), 8, 7000, 6000),
			throttle_counter:start_link('Counter6', self(), 9, 7000, 5000),
			throttle_counter:start_link('Counter7', self(), 10, 5000, 3000)
		       ],
		Check = fun({ok, Pid}) ->
				?assertNotEqual('undefined', process_info(Pid)),
				unlink(Pid),
				exit(Pid, kill)
			end,
		lists:foreach(Check, List)
	    end)
    ].

create_link_name_test_() ->
    [
     ?_test(begin
		?debugFmt("Create Link Name", []),
		Id = 'Counter1',
		{ok, Pid} = throttle_counter:start_link_name(Id, self(), 4, 5000, 10000),
		?assertNotEqual('undefined', process_info(Pid)),
		?assertEqual(Pid, whereis(Id)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		?debugFmt("Create Multiple Link Name", []),
		List = [
			throttle_counter:start_link_name('Counter1', self(), 4, 5000, 10000),
			throttle_counter:start_link_name('Counter2', self(), 5, 5000, 9000),
			throttle_counter:start_link_name('Counter3', self(), 6, 6000, 8000),
			throttle_counter:start_link_name('Counter4', self(), 7, 6000, 7000),
			throttle_counter:start_link_name('Counter5', self(), 8, 7000, 6000),
			throttle_counter:start_link_name('Counter6', self(), 9, 7000, 5000),
			throttle_counter:start_link_name('Counter7', self(), 10, 5000, 3000)
		       ],
		Check = fun({ok, Pid}) ->
				?assertNotEqual('undefined', process_info(Pid)),
				unlink(Pid),
				exit(Pid, kill)
			end,
		lists:foreach(Check, List)
	    end)
    ].

die_test_() ->
    ?_test(begin
	       ?debugFmt("Die", []),
	       {ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 1000),
	       unlink(Pid),
	       timer:sleep(1500),
	       ?assertEqual('undefined', process_info(Pid))
	   end).


check_peek_test_() ->
    [
     ?_test(?debugFmt("Check Test", [])),
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		?assertEqual({ok, 1},  throttle_counter:check(Pid)),
		?assertEqual({ok, 2},  throttle_counter:check(Pid)),
		?assertEqual({ok, 3},  throttle_counter:check(Pid)),
		?assertEqual({ok, 4},  throttle_counter:check(Pid)),
		?assertEqual({error, 4, 1000}, throttle_counter:check(Pid)),
		?assertEqual({error, 4, 1000}, throttle_counter:peek(Pid)),
		?assertEqual({error, 4, 1000}, throttle_counter:check(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		?assertEqual({ok, 1},  throttle_counter:check(Pid)),
		timer:sleep(250),
		?assertEqual({ok, 1},  throttle_counter:peek(Pid)),
		timer:sleep(750),
		?assertEqual({ok, 0},  throttle_counter:peek(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		?assertEqual({ok, 1},  throttle_counter:check(Pid)),
		timer:sleep(250),
		?assertEqual({ok, 2},  throttle_counter:check(Pid)),
		timer:sleep(750),
		?assertEqual({ok, 1},  throttle_counter:peek(Pid)),
		timer:sleep(250),
		?assertEqual({ok, 0},  throttle_counter:peek(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end)
    ].

restore_test_() ->
    [
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		?assertEqual({ok, 0},  throttle_counter:restore(Pid)),
		?assertEqual({ok, 0},  throttle_counter:peek(Pid)),
		?assertEqual({ok, 1},  throttle_counter:check(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end),
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		?assertEqual({ok, 0},  throttle_counter:restore(Pid)),
		?assertEqual({ok, 0},  throttle_counter:peek(Pid)),
		?assertEqual({ok, 1},  throttle_counter:check(Pid)),
		unlink(Pid),
		exit(Pid, kill)
	    end)
    ].

stop_test_() ->
    [
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		unlink(Pid),
		?assertEqual(ok,  throttle_counter:stop(Pid)),
		?assertEqual('undefined', process_info(Pid))
	    end),
     ?_test(begin
		{ok, Pid} = throttle_counter:start_link('Counter1', self(), 4, 1000, 10000),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		throttle_counter:check(Pid),
		unlink(Pid),
		?assertEqual(ok,  throttle_counter:stop(Pid)),
		?assertEqual('undefined', process_info(Pid))
	    end)
    ].


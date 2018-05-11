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
	       unlink(Pid),
	       exit(Pid, kill)
	   end).

create_contest_test_() ->
    [
     ?_test(begin
		?debugFmt("Create Link", []),
		throttle:start_link(),
		{ok, Pid} = throttle:start_context('context1', {2, 5000}),
		?assertNotEqual('undefined', process_info(Pid)),
		unlink(Pid),
		throttle:stop('context1')
	   end),
     ?_test(begin
		?debugFmt("Create Multiple Link", []),
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
				unlink(Pid),
				throttle:stop(Pid)
			end,
		lists:foreach(Check, List)
	    end)
    ].

check_peek_test_() ->
    [
     ?_test(?debugFmt("Check and Peek Test", [])),
     ?_test(begin
		Context = 'context1',
		Counter = 'counter1',
		throttle:start_link(),
		{ok, Pid} = throttle:start_context(Context, {2, 1000}),
		?assertEqual({ok, 1},  throttle:check(Context, Counter)),
		?assertEqual({ok, 2},  throttle:check(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:peek(Context, Counter)),
		?assertEqual({error, 2, 1000}, throttle_context:check(Context, Counter)),
		unlink(Pid),
		throttle:stop(Context)
	    end)
     %% ?_test(begin
     %% 		Name = 'counter1',
     %% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
     %% 		timer:sleep(2000),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
     %% 		unlink(Pid),
     %% 		throttle_context:stop(Pid)
     %% 	    end),
     %% ?_test(begin
     %% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter1')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter2')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter3')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter4')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter5')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter6')),
     %% 		unlink(Pid),
     %% 		throttle_context:stop(Pid)
     %% 	    end),
     %% ?_test(begin
     %% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter1')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter2')),
     %% 		?assertEqual({ok, 2},  throttle_context:check(Pid, 'counter1')),
     %% 		?assertEqual({ok, 2},  throttle_context:check(Pid, 'counter2')),
     %% 		?assertEqual({error, 2, 1000},  throttle_context:check(Pid, 'counter1')),
     %% 		?assertEqual({ok, 1},  throttle_context:check(Pid, 'counter3')),
     %% 		unlink(Pid),
     %% 		throttle_context:stop(Pid)
     %% 	    end)
    ].

%% restore_test_() ->
%%     [
%%      ?_test(begin
%% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
%% 		Name = 'counter1',
%% 	        throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 		?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
%% 		?assertEqual({ok, 0},  throttle_context:peek(Pid, Name)),
%% 		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
%% 		unlink(Pid),
%% 		throttle_context:stop(Pid)
%% 	    end),
%%      ?_test(begin
%% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
%% 		Name = 'counter1',
%% 	        throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 	        ?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
%% 		?assertEqual({ok, 0},  throttle_context:peek(Pid, Name)),
%% 		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
%% 		unlink(Pid),
%% 		throttle_context:stop(Pid)
%% 	    end),
%%      ?_test(begin
%% 		{ok, Pid} = throttle_context:start_link('context1', {2, 1000}),
%% 		Name = 'counter1',
%% 	        throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, 'counter2'),
%% 		throttle_context:check(Pid, 'counter2'),
%% 		throttle_context:check(Pid, Name),
%% 		throttle_context:check(Pid, Name),
%% 	        ?assertEqual({ok, 0},  throttle_context:restore(Pid, Name)),
%% 		?assertEqual({ok, 0},  throttle_context:peek(Pid, Name)),
%% 		?assertEqual({ok, 1},  throttle_context:check(Pid, Name)),
%% 		?assertEqual({ok, 2},  throttle_context:peek(Pid, 'counter2')),
%% 		unlink(Pid),
%% 		throttle_context:stop(Pid)
%% 	    end)
%%     ].

# Throttle

Application to implement throttling/rate limit of contexts in Erlang.

## Introduction

Throttle is a `rebar3` library, created to avoid throttling on your REST application programmed in Erlang
The application allows us to limit different context at different rates, allowing us to control the users (id)
petitions to the REST, defining the number of attempts in a time interval.
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).  %% 4 attempts every 5 seconds
{ok,<0.50.0>}
3> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id').
{ok,3}
4> throttle:check('context', 'id'), timer:sleep(1000),
4> throttle:check('context', 'id'), timer:sleep(1000),
4> throttle:check('context', 'id'), timer:sleep(5000),
4> throttle:peek('context', 'id').
{ok,1}
5> throttle:check('context', 'id'), timer:sleep(4000),
5> throttle:check('context', 'id'), timer:sleep(1000),
5> throttle:peek('context', 'id').
{ok,2}
6> throttle:check('context', 'id'), throttle:check('context', 'id'),
6> throttle:check('context', 'id'), throttle:check('context', 'id').
{ok,4}
7> throttle:check('context', 'id'), throttle:check('context', 'id'),
7> throttle:check('context', 'id'), throttle:check('context', 'id'),
7> throttle:check('context', 'id').
{error,4,5000}
```

Each user could be at different contexts at the same time being the access controller different per context.
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).  %% 4 attempts every 5 seconds
{ok,<0.50.0>}
3> throttle:start_context('context1', {6, 10000}).  %% 6 attempts every 10 seconds
{ok,<0.60.0>}
3> throttle:check('context', 'id'), throttle:check('context1', 'id'),
3> io:format("Context1: ~p, Context2: ~p~n",[throttle:peek('context', 'id'), throttle:peek('context1', 'id')]).
Context1: {ok,1}, Context2: {ok,1}
ok
4> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context1', 'id'), throttle:check('context1', 'id'), throttle:check('context1', 'id'),
4> throttle:check('context1', 'id'), throttle:check('context1', 'id'), throttle:check('context1', 'id'),
4> io:format("Context1: ~p, Context2: ~p~n",[throttle:peek('context', 'id'), throttle:peek('context1', 'id')]).
Context1: {error,4,5000}, Context2: {ok,6}
ok
```

## Set-Up
To start the application you need to call at the start to `application:start(throttle)`.
Then you can create all the context that you want calling to `throttle:start_context{'context_name', {numer_of_attemps, time_interval}}`.
Rates can also be set via application environment instead of calling `start_context` function, if you wish you could use both.
```Erlang
{throttle, [
            {resources, [
                        {'callbacks', {4, 5000}},
                        {'trades', {5, 1000}},
                        {'actions', {1000, 10000}},
                        {'authorization', {4, 5000}}
                        ]
            }]
}
```

## Functions
List of all the functions available on the library.

### start\_link() -> supervisor:startlink\_ret().
Start the throttle supervisor. Other way of starting the throttle 
application is using `application:start(throttle)`.
You can only start one throttle supervisor in your REST application.

Examples:
```Erlang
1> application:start(throttle).
ok
```
```Erlang
1> throttle:start_link().
{ok,<0.50.0>}
```

### start\_context(atom(), {integer(), integer()}) -> supervisor:startchild\_ret().
Create the context, the function receive the Id of the context, the first atom(), 
and receive a tuple, being the first integer element the number of attempts and the second
integer the time per attempts.

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.79.0>}
```

### check(atom(), atom()) -> {ok, integer()} | {error, integer(), integer()}.
Check the number of attempts of a specific user, second atom(), in a specific resource, first atom().
If the user has enough attempts return `{ok, integer()}`, being the integer the actual number of attempts in the interval.
But if the user has not attempts return `{error, integer(), integer()}`, being the first integer the number of attempts
and the second once the time of the interval.

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.79.0>}
3> throttle:check('context', 'id'),
{ok,1}
4> throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context', 'id').
{error,4,5000}
```

### peek(atom(), atom()) -> {ok, integer()} | {error, integer(), integer()}.
Check the number of attempts of a specific user, second atom(), in a specific resource, first atom() but without
updating the internal counter. 
If the user has enough attempts return `{ok, integer()}`, being the integer() the actual number of attempts in the interval.
But if the user has not attempts return `{error, integer(), integer()}`, being the first integer() the number of attempts
and the second once the time of the interval.

Peek show the same result of doing a check call but without updating the internal counter.

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.79.0>}
3> io:format("Peek: ~p, Check: ~p~n", [throttle:peek('context', 'id'), throttle:check('context', 'id')]).
Peek: {ok,1}, Check: {ok,1}
ok
```

### restore(atom(), atom()) -> {ok, integer()}.
Restore the number of attempts of a specific user, second atom(), in a specific resource, first atom(). Return {ok, 0}.
Restore of number of attempts independently of the state.

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.74.0>}
3> io:format("Check: ~p, Restore: ~p, Check:~p~n", [throttle:check('context', 'id'), 
3> throttle:restore('context', 'id'), throttle:check('context', 'id')]). 
Check: {ok,1}, Restore: {ok,0}, Check:{ok,1}
ok
```

### restart(atom()) -> ok.
Restart the counter of all the user in a context, atom().

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.74.0>}
3> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]),
Check1: {ok,1}, Chec2k:{ok,1}
ok
4> throttle:restart('context'),
ok
5> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]).
Check1: {ok,1}, Chec2k:{ok,1}
ok
```


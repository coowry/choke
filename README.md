# Throttle

Erlang application to implement throttling/rate limit in Erlang.

## Introduction

Throttle is a `rebar3` library, created to avoid throttling on your
API.

The application allows us to limit different _contexts_ at different
rates. For every context you can independently control different _identifiers_.

A use case of the library is a RESTful API. Then, you can use your resource URIs as context and users as identifiers.

## Samples

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

Each identifier could be at different contexts at the same time being the access controller different per context.
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
            {contexts, [
                        {'context1', {4, 5000}},
                        {'context2', {5, 1000}},
                        {'context3', {1000, 10000}},
                        {'context4', {4, 5000}}
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
Check the number of attempts of a specific identifier, second atom(), in a specific context, first atom().
If the identifier has enough attempts return `{ok, integer()}`, being the integer the actual number of attempts in the interval.
But if the identifier has not attempts return `{error, integer(), integer()}`, being the first integer the number of attempts
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
Check the number of attempts of a specific identifier, second atom(), in a specific context, first atom() but without
updating the internal counter. 
If the identifier has enough attempts return `{ok, integer()}`, being the integer() the actual number of attempts in the interval.
But if the identifier has not attempts return `{error, integer(), integer()}`, being the first integer() the number of attempts
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
Restore the number of attempts of a specific identifier, second atom(), in a specific context, first atom(). Return {ok, 0}.
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
Restart the counter of all the identifier in a context, atom().

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.74.0>}
3> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]).
Check1: {ok,1}, Chec2k:{ok,1}
ok
4> throttle:restart('context').
ok
5> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]).
Check1: {ok,1}, Chec2k:{ok,1}
ok
```

### stop(atom() | pid()) -> ok.
Stop the context and all his counters of the giving context.

Examples:
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).
{ok,<0.74.0>}
3> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]).
Check1: {ok,1}, Chec2k:{ok,1}
ok
4> throttle:stop('context').
ok
5> io:format("Check1: ~p, Chec2k:~p~n", [throttle:check('context', 'id1'), throttle:check('context', 'id2')]).
** exception throw: invalid_context
```

### spec stop() -> true.
Stop the throttle applications.

Examples:
```Erlang
1> application:start(throttle).
ok
2> application:stop(throttle).
ok
```
```Erlang
1> throttle:start_link().
{ok,<0.50.0>}
2> throttle:stop().
```

## Exceptions
The functions `check/2`, `peek/2`, `restore/2`, `restart/1` and `stop/1`  throw `invalid_context` when the context 
doesn't exist on the system.

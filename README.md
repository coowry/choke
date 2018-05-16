# Throttle

Application to implement throttling/rate limit of contexts in Erlang.

## Introduction

Throttle is a `rebar3` library, created to avoid throttling on your REST application programmed in Erlang
The application allow us to limit different context at different rates, allowing us to control the users (id) petitions to the REST, defining the numerber of attempts in a time interval.
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

Each user could be at different contexts at the same time beeing the access controler different per context.
```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000}).  %% 4 attempts every 5 seconds
{ok,<0.50.0>}
3> throttle:start_context('context1', {6, 10000}).  %% 6 attempts every 10 seconds
{ok,<0.60.0>}
3> throttle:check('context', 'id'), throttle:check('context1', 'id'),
3> io:format("Context1: ~p, Context2: ~p~n",[throttle:peek('context', 'id'), throttle:peek('context1', 'id')]).
"Context1: {ok,1}, Context2: {ok,1}"
4> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id'),
4> throttle:check('context1', 'id'), throttle:check('context1', 'id'), throttle:check('context1', 'id'),
4> throttle:check('context1', 'id'), throttle:check('context1', 'id'), throttle:check('context1', 'id'),
4> io:format("Context1: ~p, Context2: ~p~n",[throttle:peek('context', 'id'), throttle:peek('context1', 'id')]).
"Context1: {error,4,5000}, Context2: {ok,6}"
```

## Set-Up
To start the aplication you need to call at the start to `application:start(throttle)`.
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
List of all the functionas avilables on the library.
### start\_link() -> supervisor:startlink\_ret().
This function start the throttle supervisor. Other way of starting the throttle application is using `application:start(throttle)`.
You can only start one throttle supervisor in your REST application.

Examples:
```Erlang
1> application:start(throttle).
ok
```
```Erlang
1> throttle:start_link(throttle).
{ok,<0.50.0>}
```

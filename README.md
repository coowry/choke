# Throttle

Application to implement throttling/rate limit of contexts in Erlang.

## Introduction

Throttle is a `rebar` library, created to avoid throttling on your REST application 
writed in Erlang. 

The application allow us to control the users petitions to the REST using a internat counter
per `Id` per context. The counter, restart his internal counter every `Timeout` and if the 
counter has not been use during `Die` time, the counter die in this specific context.

```Erlang
1> application:start(throttle).
ok
2> throttle:start_context('context', {4, 5000, 10000}).
{ok,<0.50.0>}
3> throttle:check('context', 'id'), throttle:check('context', 'id'), throttle:check('context', 'id').
{ok,3}
4> throttle:check('context', 'id'), throttle:check('context', 'id'), 
4> throttle:check('context', 'id'), timer:sleep(5000),
4> throttle:peek('context', 'id').
{ok,0}
5> throttle:check('context', 'id'), throttle:check('context', 'id'), 
5> throttle:check('context', 'id'), throttle:check('context', 'id').
{ok,4}
6> throttle:check('context', 'id'), throttle:check('context', 'id'), 
6> throttle:check('context', 'id'), throttle:check('context', 'id'),
6> throttle:check('context', 'id').
{error,5000}
```

## Set-Up


## Functions

### start\_link() -> supervisor:startlink\_ret().
Start the throttle

Fink
----

Fink is an Erlang client for https://crashdump.io. Supports integration with different logging and web frameworks.

Currently supported
===================

* error_handler
* lager
* n2o

Getting started
===============

error_handler
-------------

```
>fink:add_sasl_backend().
>fink:fcatch(fun() -> 1/0 end).
```

lager
-----

```
>fink:add_lager_backend().
>lager:error("BOOM").
```

n2o
---

```
>application:set_env(n2o, erroring, n2o_fink).
>try lists:map(1, 0) catch E:R -> wf:error_page(E,R) end.
```

Example
=======

https://github.com/crashdumpio/fink-erlang/tree/master/example


Requirements
------------

Erlang 17+


Installation
============


Add to `rebar.config` file into `deps` list and run `rebar get-deps` in terminal

    {fink, ".*", {git, "git://github.com/crashdumpio/fink-erlang.git", "HEAD"}}


Configuration
=============

fink default settings

    [
     {level, info},
     {retry_interval, 5},
     {retry_times, 5},
     {protocol, http},
     {public_key, undefined},
     {secret_key, undefined},
     {project, undefined},
     {port, 31338}
    ]

To configure fink client you should add to app.config in release directory

Configure sys.config

    {fink, [{level, info},
            {retry_interval, 5},
            {retry_times, 5},
            {protocol, https},
            {public_key, "public_key"},
            {secret_key, "secret_key"},
            {project, "project_name"}]}


or configure it using `application:get_env/1,2`

    >application:load(fink),
    >application:set_env(fink, project, "project_name"),
    >application:load(start).


Configure N2O to catch all exceptions and save to crashdump
===========================================================

    {n2o, [{erroring, n2o_fink}]}.


Find more at examples page (https://github.com/crashdumpio/fink-erlang/tree/master/example)
and our blog (http://blog.crashdump.io)

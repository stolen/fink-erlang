Fink
----

Fink is an Erlang client for https://crashdump.io. Supports integration with different logging and web frameworks.

Supported frameworks
====================

* SASL (error_handler)
* Lager
* N2O
* Chicago Boss
* Nitrogen
* YAWS

Getting started
===============

SASL (error_handler) (http://www.erlang.org/doc/man/sasl_app.html)
---

```erlang
>fink:add_sasl_backend().
>error_logger:error_msg("HELP").

```

Lager (https://github.com/basho/lager)
---

```erlang
>fink:add_lager_backend().
>lager:error("BOOM").
```

N2O (https://synrc.com/apps/n2o/)
---

N2O integration module requires an additional compilation flag


```
erlc -Dfink_n2o
```
or

Add into a `rebar.config` file

```erlang
  {erl_opts, [{d, fink_n2o}]}.
```

```erlang
>application:set_env(n2o, erroring, n2o_fink).
>try lists:map(1, 0) catch E:R -> wf:error_page(E,R) end.
```

Chicago Boss (http://www.chicagoboss.org/)
---

Use lager example


Nitrogen (http://nitrogenproject.com/)
---

Use SASL example

YAWS (http://yaws.hyber.org/)
---

Use SASL example

Fink (http://docs.crashdump.io/libraries/erlang.html)
----

```erlang
>fink:fcatch(fun() -> 1/0 end).
% or
>fink:push("help, there is a bug!").
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

```erlang
{fink, ".*", {git, "git://github.com/crashdumpio/fink-erlang.git", "HEAD"}}
```

Configuration
=============

Fink default settings

```erlang
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
```

To configure fink client you should add to app.config in release directory

Configure sys.config

```erlang
{fink, [{level, info},
        {retry_interval, 5},
        {retry_times, 5},
        {protocol, https},
        {public_key, "public_key"},
        {secret_key, "secret_key"},
        {project, "project_name"}]}
```

or configure it using `application:get_env/2,3`

```erlang
>application:load(fink),
>application:set_env(fink, project, "project_name"),
>application:load(start).
```

Find more at examples page (https://github.com/crashdumpio/fink-erlang/tree/master/example)
and our blog (http://blog.crashdump.io)

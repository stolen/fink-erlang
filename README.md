Fink
----

Fink is an Erlang client for https://crashdump.io. Supports integration with different logging and web frameworks.

Currently supported:

* error_handler
* lager
* n2o

Requirements
------------

Erlang 16+




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

    {fink, [{level, "info"},
            {retry_interval, 5},
            {retry_times, 5},
            {protocol, "https"},
            {public_key, "public_key"},
            {secret_key, "secret_key"},
            {project, "project_name"}]}


or configure it using `application:get_env/1,2`

    >application:load(fink),
    >application:set_env(fink, projkect, "project_name"),
    >application:load(start).

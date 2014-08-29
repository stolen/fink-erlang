fink-erlang
-----------

Fink is an Erlang library for https://crashdump.io


Support
-------

* error_handler
* lager
* n2o


default values
--------------

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

settings
--------

Configure sys.config

    {fink, [{level, "info"},
            {retry_interval, 5},
            {retry_times, 5},
            {protocol, "https"},
            {public_key, "public_key"},
            {secret_key, "secret_key"},
            {project, "project_name"}]}


or set variables manually

    >application:load(fink),
    >application:set_env(fink, project, "project_name"),
    >application:load(start).


Configure N2O for to catch all exceptions to crashdump
------------------------------------------------------

    {n2o, [{erroring, n2o_fink}]}.


Fink more at examples page (https://github.com/crashdumpio/fink-erlang/tree/master/example)
or our blog (http://blog.crashdump.io)

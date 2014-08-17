fink-erlang
-----------

Fink is an Erlang library for https://crashdump.io


Supports
--------

* error_handler
* lager
* n2o


default values
--------------

::

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

::

  {fink, [{level, "info"},
          {retry_interval, 5},
          {retry_times, 5},
          {protocol, "https"},
          {public_key, "public_key"},
          {secret_key, "secret_key"},
          {project, "project_name"}]}


configure
---------

::

  >application:load(fink),
  >application:set_env(fink, projkect, "project_name"),
  >application:load(start).

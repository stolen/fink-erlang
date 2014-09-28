-module(fink_example_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

setup_fink() ->
    Config = [{identity, <<"x">>},
              {level, error},
              {retry_times, 5},
              {retry_interval, 3},
              {protocol, http},
              {hostname,  "localhost:8000"},
              {public_key, "88ce5225fdeff6618f7f19d31b405f8d"},
              {secret_key, "1c26593735770970d19d7e65e752212f"},
              {project, "ck_internal"}],
    [application:set_env(fink, Name, Value) || {Name, Value} <- Config],
    ok.

start() ->
    application:ensure_all_started(fink_example),
    setup_fink(),
    fink:add_sasl_handler(),
    %fink:add_lager_backend(),
    ok.

start(_StartType, _StartArgs) ->
    fink_example_sup:start_link().

stop(_State) ->
    ok.

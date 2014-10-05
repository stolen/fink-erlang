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
              {hostname,  "local.crashdump.io:8001"},
              {public_key, "43d72345f25d41a8115be4fd73aa1b01"},
              {secret_key, "2091feb7d0f04ff81a3504686b2715db"},
              {project, "python_dash_f7786e8"}],
    [application:set_env(fink, Name, Value) || {Name, Value} <- Config],
    ok.

start() ->
    application:ensure_all_started(fink_example),
    setup_fink(),
    fink:add_sasl_backend(),
    %fink:add_lager_backend(),
    ok.

start(_StartType, _StartArgs) ->
    fink_example_sup:start_link().

stop(_State) ->
    ok.

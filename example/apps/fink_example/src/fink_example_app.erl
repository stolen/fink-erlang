-module(fink_example_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

setup_fink() ->
    Config = [
      {project,    "fink_test_4e437d4"},
      {public_key, "a061b6696b38177ab4353589b60fc223"},
      {secret_key, "ca5141f4056cdd5f2501e9822616894c"}
    ],
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

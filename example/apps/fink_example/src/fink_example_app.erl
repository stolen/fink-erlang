-module(fink_example_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================



start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(inets),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(fink_example),

    {ok, Settings} = application:get_env(fink_example, lager_fink_backend),
    gen_event:add_handler(lager_event, lager_fink_backend, Settings).

start(_StartType, _StartArgs) ->
    fink_example_sup:start_link().

stop(_State) ->
    ok.

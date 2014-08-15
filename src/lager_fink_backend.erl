-module(lager_fink_backend).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").
-include_lib("lager/include/lager.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = #state{identity = proplists:get_value(id, Args),
                   level = lager_util:level_to_num(proplists:get_value(level, Args, info)),
                   retry_interval = proplists:get_value(retry_interval, Args, 5),
                   retry_times = proplists:get_value(retry_times, Args, 5),
                   protocol = proplists:get_value(protocol, Args, http),
                   public_key = proplists:get_value(public_key, Args),
                   secret_key = proplists:get_value(secret_key, Args),
                   project = proplists:get_value(project, Args),
                   port = proplists:get_value(port, Args, 31338)},
    State1 = fink_lib:connect({State#state.protocol, State}),
    {ok, State1}.

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{protocol = Protocol} = State) ->
    fink_lib:disconnect({Protocol, State}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, Level, {Date, Time}, [LevelStr, Location, Message]},
             #state{retry_times = RetryTimes} = State)
  when Level =< State#state.level ->
    spawn(fink_lib, emit, [Level, Date, Time, LevelStr, Location, Message, State, RetryTimes]),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

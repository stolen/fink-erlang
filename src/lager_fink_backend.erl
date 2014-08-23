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

init(_) ->
    State = fink_lib:new_state(),
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
    spawn(fink_lib, emit, [lager_util:num_to_level(Level), Date, Time, LevelStr, Location, Message, State, RetryTimes]),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

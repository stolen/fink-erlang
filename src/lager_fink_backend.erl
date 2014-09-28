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
         code_change/3,
         lager_message/6]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    State = fink_lib:new_connection(),
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

terminate(_Reason, State) ->
    fink_lib:disconnect(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, {lager_msg, Empty, Params, Level, {Date, Time}, S, Message} = K}, State)
->
    Allowed = lager_util:level_to_num(Level) =< lager_util:level_to_num(State#state.level),
    case Allowed of
        true -> spawn(?MODULE, lager_message, [Level, Params, Date, Time, Message, State]);
        _    -> ok
    end,
    {ok, State};

handle_event(Event, State) ->
    io:format("event ~p ~p ~n", [Event, ?MODULE]),
    {ok, State}.


%% ------------------------------------------------------------------
%% Message Generator
%% ------------------------------------------------------------------


lager_message(Level, Params, Date, Time, Message, State) ->
    io:format("log ~p~n", [?MODULE]),
    Location = prepare_location(Params),
    Msg = fink_message:prepare_message(Level, Date, Time, Location, binary:list_to_bin(Message), State),
    fink_lib:emit(Msg, State).

prepare_location(Params) ->
    Application = proplists:get_value(application, Params),
    Module      = proplists:get_value(module, Params),
    Function    = proplists:get_value(function, Params),
    Line        = proplists:get_value(line, Params),
    io_lib:format("~p/~p:~p:~p", [Application, Module, Function, Line]).

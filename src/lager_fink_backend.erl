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
    State1 = fink_lib:connect(State),
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

handle_event({log, {lager_msg, _Data, Params, Level, {Date, Time}, _Timestamp, Message} = _M}, State) when Level =:= State#state.level ->
    spawn(?MODULE, lager_message, [Level, Params, Date, Time, Message, State]),
    {ok, State};

handle_event(_Event, State) ->
    % ignore
    {ok, State}.


%% ------------------------------------------------------------------
%% Message Generator
%% ------------------------------------------------------------------


lager_message(Level, Params, Date, Time, Msg, State) ->
    Location = prepare_location(Params),
    Content = [{<<"message">>, binary:list_to_bin(Msg)}],
    Message = fink_message:prepare_message(Level, Date, Time, Location, Content, State),
    fink_lib:emit(Message, State).

prepare_location(Params) ->
    Application = proplists:get_value(application, Params),
    Module      = proplists:get_value(module, Params),
    Function    = proplists:get_value(function, Params),
    Line        = proplists:get_value(line, Params),
    io_lib:format("~p/~p:~p:~p", [Application, Module, Function, Line]).

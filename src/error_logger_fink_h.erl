-module(error_logger_fink_h).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         error_logger_message/3]).


%%======================================
%% gen_event Function Definitions
%%======================================

init(_Args) ->
    process_flag(trap_exit, true),
    State = fink_lib:new_connection(),
    State1 = fink_lib:connect({State#state.protocol, State}),
    {ok, State1}.

handle_event({error, _Leader, Msg}, #state{error = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State};
handle_event({info_msg, _Leader, Msg}, #state{info_msg = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State};
handle_event({warning_msg, _Leader, Msg}, #state{warning_msg = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State};

handle_event({error_report, _Leader, Msg}, #state{error_report = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State};
handle_event({info_report, _Leader, Msg}, #state{info_report = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State};
handle_event({warning_report, _Leader, Msg}, #state{warning_error = Level} = State) ->
    R = ?MODULE:error_logger_emit(Level, Msg, State),
    {R, State}.

% {PID, Msg, Data}

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, _File, _Type}) ->
    remove_handler;
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    fink_lib:disconnect(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Message Generator
%% ------------------------------------------------------------------

error_logger_message(Level, {_PID, Msg, Params}, State) ->
    % Msg pattern, Params - a list of values
    Val = case Params of
              [] -> io_lib:format(Msg, Params);
              _  -> Msg
          end,
    Message = fink_message:prepare_message(Level, "", Val, [], State),
    fink_lib:emit(Level, "", Message, State).

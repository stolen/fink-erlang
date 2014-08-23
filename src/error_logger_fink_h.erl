-module(error_logger_fink_h).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([add_handler/0, add_handler/1]).

%% ------------------------------------------------------------------
%% gen_event Function Exports
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

add_handler(Args) -> error_logger:add_report_handler(?MODULE, Args).
add_handler()     -> error_logger:add_report_handler(?MODULE, []).

%%======================================
%% gen_event Function Definitions
%%======================================

init(_Args) ->
    process_flag(trap_exit, true),
    State = fink_lib:new_state(),
    State1 = fink_lib:connect({State#state.protocol, State}),
    {ok, State1}.

handle_event({error, _Leader, Msg}, #state{error = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
    {R, State};
handle_event({info_msg, _Leader, Msg}, #state{info_msg = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
    {R, State};
handle_event({warning_msg, _Leader, Msg}, #state{warning_msg = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
    {R, State};

handle_event({error_report, _Leader, Msg}, #state{error_report = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
    {R, State};
handle_event({info_report, _Leader, Msg}, #state{info_report = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
    {R, State};
handle_event({warning_report, _Leader, Msg}, #state{warning_error = Level} = State) ->
    R = fink_lib:logger_emit(Level, Msg, State),
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

terminate(_Reason, #state{protocol = Protocol} = State) ->
    fink_lib:disconnect({Protocol, State}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    State = #state{},
    State1 = fink_lib:connect({State#state.protocol, State}),
    {ok, State1}.

handle_event({error, Leader, Msg}, #state{error = L} = State) ->
    R = fink_lib:logger_emit(error, Leader, L, Msg),
    {R, State};
handle_event({info_msg, Leader, Msg}, #state{info_msg = L} = State) ->
    R = fink_lib:logger_emit(info_msg, Leader, L, Msg),
    {R, State};
handle_event({warning_msg, Leader, Msg}, #state{warning_msg = L} = State) ->
    R = fink_lib:logger_emit(warning_msg, Leader, L, Msg),
    {R, State};

handle_event({error_report, Leader, Msg}, #state{error_report = L} = State) ->
    R = fink_lib:logger_emit(error_msg, Leader, L, Msg),
    {R, State};
handle_event({info_report, Leader, Msg}, #state{info_report = L } = State) ->
    R = fink_lib:logger_emit(info_report, Leader, L, Msg),
    {R, State};
handle_event({warning_report, Leader, Msg}, #state{warning_error = L} = State) ->
    R = fink_lib:logger_emit(warning_report, Leader, L, Msg),
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

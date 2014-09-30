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
    State1 = fink_lib:connect(State),
    {ok, State1}.

handle_event({error, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [error, Msg, State]),
    {ok, State};
handle_event({info_msg, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [info, Msg, State]),
    {ok, State};
handle_event({warning_msg, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [warning, Msg, State]),
    {ok, State};

handle_event({error_report, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [error, Msg, State]),
    {ok, State};
handle_event({info_report, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [info, Msg, State]),
    {ok, State};
handle_event({warning_report, _Leader, Msg}, State) ->
    spawn(?MODULE, error_logger_message, [warning, Msg, State]),
    {ok, State}.

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

error_logger_message(Level, {_PID, crash_report, [Report, _Smtng]}, State) when Level =:= State#state.level ->
    {Message, Stacktrace} = format_stacktrace(proplists:get_value(error_info, Report, [])),
    Report1 = format_report(Report),
    Content = [{<<"message">>, Message},
               {<<"error_report">>, Report1},
               {<<"stacktrace">>, Stacktrace}],
    error_logger_emit(Level, "", Content, State),
    ok;

% Msg pattern, Params - a list of values
error_logger_message(Level, {_PID, Msg, Params}, State) when Level =:= State#state.level ->
    Content = [{<<"message">>, format_value(Msg, Params)}],
    error_logger_emit(Level, "", Content, State),
    ok;
error_logger_message(_Level, _Msg, _State) ->
    %io:format("missed report: ~p ~p~n", [Level, Msg]),
    ok.
error_logger_emit(Level, Location, Content, State) ->
    Message = fink_message:prepare_message(Level, Location, Content, State),
    fink_lib:emit(Message, State),
    ok.


format_stacktrace({Error, Reason, Stacktrace}) ->
    {fink_stacktrace:stacktrace_title(Error, Reason, Stacktrace),
     fink_stacktrace:stacktrace_body(Stacktrace)};
format_stacktrace([]) ->
    {<<"">>, <<"">>}.


format_value(Msg, [])     -> io_lib:format(Msg, []);
format_value(Msg, _Params) -> Msg.


format_report(Out)     -> format_report([], Out).
format_report(Out, []) -> list_to_binary(Out);
format_report(Out, [{initial_call, {Module, Func, Args}}|Report]) ->
    FRow = io_lib:format("~p: ~p/~p~n", [Module, Func, length(Args)]),
    format_report(Out ++ FRow, Report);
format_report(Out, [{error_info, _Value}|Report]) ->
    FRow = "",
    format_report(Out ++ FRow, Report);
format_report(Out, [{Field, Value}|Report]) ->
    FRow = io_lib:format("~p: ~p~n", [Field, Value]),
    format_report(Out ++ FRow, Report).

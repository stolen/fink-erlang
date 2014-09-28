-module(fink).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").

-compile(export_all).

add_lager_backend()    -> gen_event:add_handler(lager_event, lager_fink_backend, []).

add_sasl_handler(Args) -> error_logger:add_report_handler(error_logger_fink_h, Args).
add_sasl_handler()     -> error_logger:add_report_handler(error_logger_fink_h, []).


% catch function -> Result | error
fcatch(Fun) ->
    fcatch(Fun, fun(_, _, _) -> error end).

% catch function or eval 2nd function
fcatch(Fun, OnError) ->
    try
        Fun()
    catch Error:Reason ->
        [_, _, Stacktrace] = ?MODULE:stacktrace(Error, Reason),
        OnError(Error, Reason, Stacktrace)
    end.

stacktrace(Error, Reason) ->
    {Stacktrace, Message} = fink_stacktrace:stacktrace(Error, Reason),
    ?MODULE:push(Message),
    [Error, Reason, Stacktrace].

push(Message) ->
    spawn(fun() ->
        State = fink_lib:new_connection(),
        Conn = fink_lib:connect({State#state.protocol, State}),
        fink_lib:emit(State#state.level, <<"">>, Message, Conn)
    end).


% fink:stacktrace(error, undef).
% fink:fcatch(fun() -> 1/0 end).

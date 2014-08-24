-module(fink).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


-compile(export_all).


fcatch(Fun) ->
    fcatch(Fun, fun(_, _, _) -> error end).

fcatch(Fun, OnError) ->
    try
        Fun()
    catch Error:Reason ->
        {_, _, Stacktrace} = ?MODULE:push(Error, Reason),
        OnError(Error, Reason, Stacktrace)
    end.

push(Error, Reason) ->
    {Stacktrace, Message} = fink_lib:message(Error, Reason),
    ?MODULE:emit(Message),
    [Error, Reason, Stacktrace].

emit(Message) ->
    emit(Message, <<"">>).

emit(Message, Location) ->
    spawn(fun() ->
        State = fink_lib:new_state(),
        CState = fink_lib:connect({State#state.protocol, State}),
        fink_lib:emit(State#state.level, Location, Message, CState)
    end).

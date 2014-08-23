-module(fink).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


-compile(export_all).

message(throw, Reason, _) ->
    io_lib:format("throw(~p)", [Reason]);
message(error, Type, Stacktrace) ->
    [{Module, Operator, Args, _}|_Stack] = Stacktrace,
    Args1 = lists:last(io_lib:format("~p", [Args])),
    Args2 = string:sub_string(Args1, 2, string:len(Args1) - 1),
    Msg = io_lib:format("~s - ~p:~p(~s)~n", [Type, Module, Operator, Args2]),
    re:replace(Msg, "\n", "", [global]);
message(Error, Reason, _Sx) ->
    io_lib:format("~s -> ~s", [Error, Reason]).

fcatch(Fun) ->
    fcatch(Fun, fun(_, _, _) -> error end).

fcatch(Fun, OnError) ->
    try
        Fun()
    catch Error:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        Msg = message(Error, Reason, Stacktrace),
        Message = [{title,  list_to_binary(Msg)},
                   {error,  list_to_binary(io_lib:format("~s", [Error]))},
                   {reason, list_to_binary(io_lib:format("~s", [Reason]))},
                   {stacktrace, list_to_binary(io_lib:format("~p", [Stacktrace]))}],
        ?MODULE:push(Message),
        OnError(Error, Reason, Stacktrace)
    end.


push(Message) ->
    push(Message, <<"">>).

push(Message, Location) ->
    spawn(fun() ->
        NState = fink_lib:new_state(),
        State = fink_lib:connect({NState#state.protocol, NState}),
        fink_lib:emit(State#state.level, Location, Message, State)
    end).

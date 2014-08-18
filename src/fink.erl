-module(fink).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


-compile(export_all).


fcatch(Fun) ->
    fcatch(Fun, fun() -> error end).

fcatch(Fun, Success) ->
    try
        Fun()
    catch E:R ->
        Message = [{title, ""},
                   {error, io_lib:format("~p", [E])},
                   {reason, io_lib:format("~p", [R])},
                   {stacktrace, binary:list_to_bin(io_lib:format("~p", [erlang:get_stacktrace()]))}],
        ?MODULE:push(Message),
        Success()
    end.


push(Message) ->
    push(Message, <<"">>).

push(Message, Location) ->
    spawn(fun() ->
        NState = fink_lib:new_state(),
        State = fink_lib:connect({NState#state.protocol, NState}),
        fink_lib:emit(State#state.level, Location, Message, State)
    end).

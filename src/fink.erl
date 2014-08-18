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
        Message = io_lib:format("{~p, ~p, ~p}", [E, R, erlang:get_stacktrace()]),
        ?MODULE:push(Message),
        Success()
    end.


push(Message) -> push(Message, <<"">>).

push(Message, Location) ->
    NState = fink_lib:new_state(),
    State = fink_lib:connect({NState#state.protocol, NState}),
    fink_lib:emit(State#state.level, Location, Message, State).


%io:format("~s", [fink:push(<<"yi">>)]).

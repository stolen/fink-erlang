-module(fink).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


-compile(export_all).


message(Message) ->
    message(Message, [], {undefined, undefined}).

message(Message, Params) ->
    message(Message, Params, {undefined, undefined}).

message(Message, Params, {Module, Line}) ->
    %io:format("~p ~p ~p~n", [erlang:get_stacktrace(), ?MODULE:module_info(), process_info(self(), backtrace)]).
    try
        1/0
        %throw("121")
    catch E:R ->
            %% {_, E} = process_info(self(), backtrace),
            %% io:format("~p ~s~n", [ E])
            io:format("~p ~p ~p~n", [E, R, erlang:get_stacktrace()])
    end.

fcatch(Fun) ->
    try
        Fun()
    catch E:R ->
        _Module = [], % ?MODULE:module_info(),
        % Process = process_info(self(), current_stacktrace),
        io:format("~p ~p ~p ~p ~p~n", [
                                       E, R, erlang:get_stacktrace(), ?LINE, ?MODULE
                                      ]),
        error
    end.


push(Message) -> push(Message, <<"">>).

push(Message, Location) ->
    NState = fink_lib:new_state(),
    State = fink_lib:connect({NState#state.protocol, NState}),
    fink_lib:emit(State#state.level, Location, Message, State).


%io:format("~s", [fink:push(<<"yi">>)]).

-module(fink_stacktrace).

-export([stacktrace/2]).


stacktrace(Error, Reason) ->
    Stacktrace = erlang:get_stacktrace(),
    Message =  list_to_binary(stacktrace_title(Error, Reason, Stacktrace)),
    Stckrs = list_to_binary(io_lib:format("~s", [stacktrace_body(Stacktrace)])),
    {Stacktrace, {stacktrace, Message, Stckrs}}.


format_stacktrace([])                 -> [];
format_stacktrace([Section|Stacktrace]) ->
    {Module, Func, Arity, [{file, File}, {line, Line}]} = Section,
    Output = [atom_to_list(Module), <<":">>, atom_to_list(Func), <<"/">>, integer_to_binary(Arity),
              <<" (">>, File, <<":">>, integer_to_binary(Line), <<")">>, <<"\n">>],
    [Output|format_stacktrace(Stacktrace)].

stacktrace_body([]) ->
    [];
stacktrace_body([_|Stacktrace]) ->
    format_stacktrace(Stacktrace).

stacktrace_title(Error, Reason, []) ->
    io_lib:format("~s -> ~s", [Error, Reason]);
stacktrace_title(throw, Reason, _) ->
    io_lib:format("throw(~p)", [Reason]);
stacktrace_title(error, Type, Stacktrace) ->
    [{Module, Operator, Args, _}|_Stack] = Stacktrace,
    Args1 = lists:last(io_lib:format("~p", [Args])),
    Args2 = string:sub_string(Args1, 2, string:len(Args1) - 1),
    Msg = io_lib:format("~s - ~p:~p(~s)~n", [Type, Module, Operator, Args2]),
    re:replace(Msg, "\n", "", [global]);
stacktrace_title(Error, Reason, _Sx) ->
    io_lib:format("~s -> ~s", [Error, Reason]).

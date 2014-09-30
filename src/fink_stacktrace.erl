-module(fink_stacktrace).

-export([stacktrace/2]).

-export([stacktrace_title/3,
         stacktrace_body/1]).


stacktrace(Error, Reason) ->
    Stacktrace = erlang:get_stacktrace(),
    Message = stacktrace_title(Error, Reason, Stacktrace),
    Stckrs = stacktrace_body(Stacktrace),
    {Stacktrace, {stacktrace, Message, Stckrs}}.


format_stacktrace([])                 -> [];
format_stacktrace([{Module, Func, Arity, [{file, File}, {line, Line}]}|Stacktrace]) ->
    Output = [atom_to_list(Module), <<":">>, atom_to_list(Func), <<"/">>, integer_to_binary(Arity),
              <<" (">>, File, <<":">>, integer_to_binary(Line), <<")">>, <<"\n">>],
    [Output|format_stacktrace(Stacktrace)];
format_stacktrace([{Module, Func, Args, _Params}|Stacktrace]) ->
    Args1 = lists:last(io_lib:format("~p", [Args])),
    Args2 = string:sub_string(Args1, 2, string:len(Args1) - 1),
    Output = [
              atom_to_list(Module), <<":">>, atom_to_list(Func), <<"(">>, Args2, <<")">>, <<"\n">>
             ],
    [Output|format_stacktrace(Stacktrace)].


stacktrace_body(Stacktrace) ->
    list_to_binary(io_lib:format("~s", [fstacktrace_body(Stacktrace)])).

fstacktrace_body([]) ->
    [];
fstacktrace_body(Stacktrace) ->
    format_stacktrace(Stacktrace).


stacktrace_title(Error, Reason, Stacktrace) ->
    list_to_binary(fstacktrace_title(Error, Reason, Stacktrace)).

fstacktrace_title(Error, Reason, []) ->
    io_lib:format("~s -> ~s", [Error, Reason]);
fstacktrace_title(throw, Reason, _) ->
    io_lib:format("throw(~p)", [Reason]);
fstacktrace_title(error, Type, Stacktrace) ->
    [{Module, Operator, Args, _}|_Stack] = Stacktrace,
    Args1 = lists:last(io_lib:format("~p", [Args])),
    Args2 =  case length(Args1) of
                 1 -> Args1;
                 0 -> Args1;
                 _ -> string:sub_string(Args1, 2, string:len(Args1) - 1)
             end,
    Msg = io_lib:format("~s - ~p:~p(~s)~n", [Type, Module, Operator, Args2]),
    re:replace(Msg, "\n", "", [global]);
fstacktrace_title(Error, Reason, _Sx) ->
    io_lib:format("~s -> ~s", [Error, Reason]).

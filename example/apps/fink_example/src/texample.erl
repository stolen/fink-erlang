-module(texample).
-compile(export_all).

lg()  -> lager:error("YO").
lg1() -> lager:info("YO").


sasl()  -> error_logger:error_msg("HALP error").
sasl1() -> error_logger:info_msg("HALP info").


lager_messages() ->
    lg(),
    lg1().

sasl_messages() ->
    sasl(),
    sasl1().

sasl_reports() ->
    proc_lib:spawn(fun() -> process_flag(trap_exit, true), 1/0 end),
    proc_lib:spawn(fun() -> process_flag(trap_exit, true), throw(start_error) end),
    proc_lib:spawn(fun() -> process_flag(trap_exit, true), throw(child_terminated) end),
    proc_lib:spawn(fun() -> process_flag(trap_exit, true), throw(shutdown_error) end),
    proc_lib:spawn(fun() ->
                           process_flag(trap_exit, true),
                           proc_lib:spawn_link(fun() ->
                                                       1/0
                                               end)
                   end).


%% texample:lg().
%% texample:lg1().
%% texample:sasl().
%% texample:sasl1().
%% texample:sasl_reports().



% fink:stacktrace(error, undef).
% fink:fcatch(fun() -> 1/0 end).

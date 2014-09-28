-module(texample).
-compile(export_all).

lg()  -> lager:error("YO").
lg1() -> lager:info("YO").


sasl()  -> error_logger:error_msg("HALP error").
sasl1() -> error_logger:info_msg("HALP info").

%% texample:lg().
%% texample:lg1().
%% texample:sasl().
%% texample:sasl1().



% fink:stacktrace(error, undef).
% fink:fcatch(fun() -> 1/0 end).

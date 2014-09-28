-module(texample).
-compile(export_all).

lg() ->
    lager:error("YO").

lg1() ->
    lager:info("YO").


sasl() ->
    error_logger:error_msg("HALP").

%% texample:lg().
%% texample:sasl().

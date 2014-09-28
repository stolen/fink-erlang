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


%% {log,{lager_msg,[], [{pid,<0.7.0>}], info,
%%                       {["2014",45,"09",45,"28"],
%%                        ["00",58,"58",58,"25",46,"054"]},
%%                       {1411,891105,54516},
%%                       [65,112,112,108,105,99,97,116,105,111,110,32,"lager",32,
%%                        115,116,97,114,116,101,100,32,111,110,32,110,111,100,
%%                        101,32,"nonode@nohost"]}}



%% {lager_msg,[], [{pid,<0.70.0>},
%%  {line,94},
%%  {file,"src/lager_handler_watcher.erl"},
%%  {module,lager_handler_watcher}], debug, {["2014",45,"09",45,"28"],["01",58,"05",58,"12",46,"463"]}, {1411,891512,463771}, [76,97,103,101,114,32,105,110,115,116,97,108,108,101,100,
%%  32,104,97,110,100,108,101,114,32,"lager_backend_throttle",
%%  32,105,110,116,111,32,"lager_event"]}

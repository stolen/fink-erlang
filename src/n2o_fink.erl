-module(n2o_fink).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-export(?FAULTER_API).

stack() -> n2o_error:stack().
error_page(Class, Error) -> n2o_error:error_page(Class, Error).

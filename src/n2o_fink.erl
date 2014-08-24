-module(n2o_fink).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-export(?FAULTER_API).

stack(Error, Reason) ->
    fink:push(Error, Reason),
    n2o_error:stack(Error, Reason).

error_page(Class, Error) ->
    fink:push(Class, Error),
    n2o_error:error_page(Class, Error).

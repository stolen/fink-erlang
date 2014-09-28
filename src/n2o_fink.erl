-module(n2o_fink).
-compile(export_all).
-export([stack/2, error_page/2]).

stack(Error, Reason) ->
    fink:stacktrace(Error, Reason),
    n2o_error:stack(Error, Reason).

error_page(Class, Error) ->
    fink:stacktrace(Class, Error),
    n2o_error:error_page(Class, Error).

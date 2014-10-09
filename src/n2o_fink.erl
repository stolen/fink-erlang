-module(n2o_fink).
-compile(export_all).
-export([stack/2, error_page/2]).

% API

stack(Error, Reason) ->
    fink:stacktrace(Error, Reason, get_custom_info()),
    n2o_error:stack(Error, Reason).

error_page(Class, Error) ->
    fink:stacktrace(Class, Error, get_custom_info()),
    n2o_error:error_page(Class, Error).

% Internal
get_custom_info() ->
    [
     {<<"request">>, get_request_info()}
    ].

get_request_info() ->
    Req = wf_context:context(),
    {{Session, _}, _, _, _, _} = Req#cx.session,
    {Headers, _} = wf:headers(Req#cx.req),
    {RawPeer, _} = wf:peer(Req#cx.req),
    {Method, _} = cowboy_req:method(Req#cx.req),
    {Host, _} = cowboy_req:host(Req#cx.req),
    {Port, _} = cowboy_req:port(Req#cx.req),
    {Path, _} = cowboy_req:path(Req#cx.req),
    {Qs, _} = cowboy_req:qs(Req#cx.req),
    Protocol = case Port of 443 -> "https"; _ -> "http" end,
    Peer = list_to_binary(inet_parse:ntoa(RawPeer)),
    Url = list_to_binary(io_lib:format("~s://~s:~w~s?~s",
                                       [Protocol, Host, Port, Path, Qs])),
    [{<<"path">>, Url},
     {<<"method">>, Method},
     {<<"args_get">>, Req#cx.params},
     {<<"args_post">>, Req#cx.form},
     {<<"module">>, Req#cx.module},
     {<<"session">>, Session},
     {<<"peer">>, Peer},
     {<<"headers">>, Headers}].

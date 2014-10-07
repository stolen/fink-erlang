-module(fink_lib).

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").

-compile(export_all).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

iso_8601_fmt(DateTime) ->
    % https://erlangcentral.org/wiki/index.php?title=Converting_Between_struct:time_and_ISO8601_Format
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

get_settings(Name)          ->
    get_settings(Name, undefined).
get_settings(Name, Default) ->
    case application:get_env(fink, Name, Default) of {ok, Value} -> Value; V -> V end.

%% ------------------------------------------------------------------
%% Sender Function Definitions
%% ------------------------------------------------------------------


emit(Message, State) ->
    emit(Message, State, State#state.retry_times).

emit(_Message, _State, 0) -> ok;
emit(Message, State, Rt) ->
    case push(State#state.protocol, Message, State) of
        ok         -> ok;
        {error, _} -> timer:sleep(State#state.retry_interval * 1000),
                      emit(Message, State, Rt - 1)
    end.

push(udp, Message, #state{socket = Socket} = State) ->
    Msg = io_lib:format("~s\n\n~s", [auth_header(State), Message]),
    case gen_udp:send(Socket, State#state.hostname, 31337, Msg) of
        {error, Reason} -> push_error("udp", Reason), {error, Reason};
        _               -> ok
    end;

push(Protocol, Message, #state{url = Url} = State) ->
    Headers = [{"X-Auth", auth_header(State)}],
    Request = {Url, Headers, "application/json", Message},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, Resp}      -> %io:format("Success: ~p~n", [Resp]),
                           ok;
        {error, Reason} -> push_error(Protocol, Reason), {error, Reason}
    end.

push_error(Protocol, Reason) ->
    io:format("Fink error. Sending[~s] fail: Reason: ~p~n", [Protocol, Reason]).


%% ------------------------------------------------------------------
%% Connection Function Definitions
%% ------------------------------------------------------------------

auth_header(#state{public_key = PublicKey, secret_key = SecretKey}) ->
    io_lib:format(
      "Crashdump fink_ts=~p, fink_client=~s, fink_pkey=~s, fink_skey=~s",
      [calendar:datetime_to_gregorian_seconds(calendar:universal_time()), ?CLIENT, PublicKey, SecretKey]
    ).

new_connection() ->
    #state{identity       = get_settings(id, <<"erlang-fink">>),
           level          = get_settings(level, error),
           retry_interval = get_settings(retry_interval, 5),
           retry_times    = get_settings(retry_times, 5),
           protocol       = get_settings(protocol, https),
           public_key     = get_settings(public_key),
           secret_key     = get_settings(secret_key),
           project        = get_settings(project),
           hostname       = get_settings(hostname, "crashdump.io"),
           port           = get_settings(port, 31337)}.

connect(#state{protocol = Protocol} = State) ->
    connect({Protocol, State});

connect({udp, #state{port = Port} = State}) ->
    case gen_udp:open(Port, [binary, {active, true}]) of
        {ok, Socket}        ->
            State#state{socket = Socket};
        {error, eaddrinuse} ->
            connect({udp, State#state{port = Port + 1}});
        {error, Reason}     ->
            io:format("Fink error. Connection[udp] fail: Reason: ~p~n", [Reason]),
            connect({https, State#state{protocol = https}})
    end;
connect({http, #state{project = Project} = State}) ->
    Url = io_lib:format("http://~s/api/~s/~s/push", [State#state.hostname, ?API_VERSION, Project]),
    State#state{url = binary_to_list(list_to_binary(Url))};
connect({https, #state{project = Project} = State}) ->
    Url = io_lib:format("https://~s/api/~s/~p/push", [State#state.hostname, ?API_VERSION, Project]),
    State#state{url = binary_to_list(list_to_binary(Url))}.

disconnect(#state{protocol = Protocol} = State) ->
    disconnect(Protocol, State).

disconnect(udp, #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        Socket1 -> gen_udp:close(Socket1), ok
    end;
disconnect(_Protocol, _State) ->
    ok.

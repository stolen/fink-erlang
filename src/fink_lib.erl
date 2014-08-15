-module(fink_lib).

-define(CLIENT, "lager_fink_backend/0.0.1").
%-define(HOST, "crashkeeper.com/api").
-define(HOST, "localhost/api").

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


-export([
         prepare_message/0,
         prepare_message/7,
         logger_emit/4,
         emit/9,
         emit/8,
         connect/1,
         disconnect/1,
         auth_header/3
        ]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

prepare_message(Level, Date, Time, _LevelStr, _Location, Message, #state{project = Project} = _State) ->
    % int, str, str, str, pid, message, #state
    Datetime = io_lib:format("~s ~s", [Date, Time]),
    {ok, Paths} = erl_prim_loader:get_path(),
    {Platform, Kernel} = os:type(),
    base64:encode(zlib:compress(jsx:to_json([{level,       atom_to_binary(lager_util:num_to_level(Level), latin1)},
                                             {platrofm,    <<"erlang">>},
                                             {paths,       Paths},
                                             %% {module_info, os:module_info()},
                                             %% {stacktrace,  erlang:get_stacktrace()},
                                             {version,     binary:list_to_bin(element(1, init:script_id()))},
                                             {memory,      erlang:memory()},
                                             {node,        atom_to_binary(erlang:node(), latin1)},
                                             {pid,         binary:list_to_bin(os:getpid())},
                                             {os_type,     io_lib:format("~s~s", [Platform, Kernel])},
                                             {pwd,         binary:list_to_bin(os:getenv("PWD"))},
                                             {project,     binary:list_to_bin(Project)},
                                             {datetime,    binary:list_to_bin(Datetime)},
                                             {message,     binary:list_to_bin(Message)}]))).

prepare_message() ->
    lager:log(info, 100, "test").


logger_emit(Type, Leader, L, {_PID, _Msg, _Data} = M) ->
    io:format("~p ~p ~p ~p", [Type, Leader, L, M]),
    ok.


emit(_, _, _, _, _, _, _State, 0) ->
    ok;
emit(Level, Date, Time, LevelStr, Location, Message, #state{retry_interval = RetryInterval, protocol = Protocol} = State, Rt) ->
    case emit(protocol, Protocol, Level, Date, Time, LevelStr, Location, Message, State) of
        ok -> ok;
        {error, _} ->
            timer:sleep(RetryInterval * 1000),
            emit(Level, Date, Time, LevelStr, Location, Message, State, Rt - 1)
    end.

emit(protocol, udp, Level, Date, Time, LevelStr, Location, Message, #state{socket = Socket} = State) ->
    M = prepare_message(Level, Date, Time, LevelStr, Location, Message, State),
    Msg = io_lib:format("~s\n\n~s", [auth_header(Date, Time, State), M]),
    case gen_udp:send(Socket, ?HOST, 31337, Msg) of
        {error, Reason} ->
            io:format("Error fink sending: Reason: ~w State: ~p~n", [Reason, State]),
            {error, Reason};
        _               -> ok
    end;

emit(protocol, _, Level, Date, Time, LevelStr, Location, Message, #state{url = Url} = State) ->
    Headers = [{"X-Auth", auth_header(Date, Time, State)}],
    Msg = prepare_message(Level, Date, Time, LevelStr, Location, Message, State),
    Request = {Url, Headers, "application/json", Msg},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, _}         -> ok;
        {error, Reason} -> {error, Reason}
    end.

auth_header(_Date, _Time, #state{public_key = PublicKey, secret_key = SecretKey} = _State) ->
    io_lib:format(
      "Crashdump fink_ts=~p, fink_client=~s, fink_pkey=~s, fink_skey=~s",
      [calendar:datetime_to_gregorian_seconds(calendar:universal_time()), ?CLIENT, PublicKey, SecretKey]
    ).



connect({udp, #state{port = Port} = State}) ->
    case gen_udp:open(Port,
                      [binary, {active, true}]) of
        {ok, Socket} ->
            State#state{socket = Socket};
        {error, eaddrinuse} ->
            connect({udp, State#state{port = Port + 1}});
        {error, Reason} ->
            io:format("Error fink connect using udp: Reason: ~w State: ~p~n", [Reason, State]),
            connect({http, State#state{protocol = http}})
    end;
connect({http, #state{public_key = PublicKey, secret_key = SecretKey, project = Project} = State}) ->
    State#state{url = io_lib:format("http://~s:~s@~s/~s/push", [PublicKey, SecretKey, ?HOST, Project])};
connect({https, #state{public_key = PublicKey, secret_key = SecretKey, project = Project} = State}) ->
    State#state{url = io_lib:format("https://~s:~s@~s/~s/push", [PublicKey, SecretKey, ?HOST, Project])}.

disconnect({udp, #state{socket = Socket} = _State}) ->
    case Socket of
        undefined -> ok;
        Socket1 -> gen_udp:close(Socket1), ok
    end;
disconnect({_, _State}) ->
    ok.

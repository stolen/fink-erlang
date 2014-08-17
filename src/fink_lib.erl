-module(fink_lib).

-define(CLIENT, "lager_fink_backend/0.0.1").

%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include("fink.hrl").


%% -export([
%%          new_state/0,
%%          prepare_message/0,
%%          prepare_message/7,
%%          logger_emit/4,
%%          emit/8,
%%          emit/9,
%%          connect/1,
%%          disconnect/1,
%%          auth_header/3,
%%          get_settings/1,
%%          get_settings/2
%%         ]).

-compile(export_all).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

iso_8601_fmt(DateTime) ->
    % https://erlangcentral.org/wiki/index.php?title=Converting_Between_struct:time_and_ISO8601_Format
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

get_settings(Name)          -> get_settings(Name, undefined).
get_settings(Name, Default) ->
    case application:get_env(fink, Name, Default) of
        {ok, Value} -> Value;
        V           -> V
    end.

new_state() ->
    #state{
       identity       = get_settings(id),
       level          = list_to_atom(get_settings(level, "info")),
       retry_interval = get_settings(retry_interval, 5),
       retry_times    = get_settings(retry_times, 5),
       protocol       = list_to_atom(get_settings(protocol, "https")),
       public_key     = get_settings(public_key),
       secret_key     = get_settings(secret_key),
       project        = get_settings(project),
       hostname       = get_settings(hostname),
       port           = get_settings(port, 31338)
      }.

prepare_message(Level, Location, Message, State) ->
    prepare_message(Level, Location, Message, [], State).

prepare_message(Level, Location, Message, Params, State) ->
    Datetime = iso_8601_fmt(erlang:localtime()),
    prepare_message({Level, Datetime, Location, Message, Params, State}).

prepare_message(Level, Date, Time, Location, Message, State) ->
    Datetime = io_lib:format("~s ~s", [Date, Time]),
    prepare_message({Level, Datetime, Location, Message, [], State}).

prepare_message({Level, Datetime, _Location, Message, Params, #state{project = Project}}) ->
    %% {ok, Paths} = erl_prim_loader:get_path(),
    %% {Platform, Kernel} = os:type(),
    {ok, Hostname} = inet:gethostname(),
    Extra = [
               {memory,      erlang:memory()},
               {node,        atom_to_binary(erlang:node(), latin1)},
               {pid,         binary:list_to_bin(os:getpid())}
              ] ++ Params,
    base64:encode(zlib:compress(
                    jsx:encode([{level,     atom_to_binary(Level, latin1)},
                                            {platform,    <<"erlang">>},
                                            %% {paths,       lists:map(fun list_to_binary/1, Paths)},
                                            %% {module_info, os:module_info()},
                                            %% {stacktrace,  erlang:get_stacktrace()},
                                            {server_name, binary:list_to_bin(Hostname)},
                                            {version,     binary:list_to_bin(element(1, init:script_id()))},

                                            %% {os_type,     io_lib:format("~s~s", [Platform, Kernel])},
                                            {pwd,         binary:list_to_bin(os:getenv("PWD"))},
                                            {project,     binary:list_to_bin(Project)},
                                            {datetime,    binary:list_to_bin(Datetime)},
                                            {extra,       Extra},
                                            {message,     Message}])
                   ))
.

logger_emit(Level, {_PID, Msg, Params}, State) ->
    Val = case Params of
              [] -> io_lib:format(Msg, Params);
              _  -> Msg
          end,
    Message = prepare_message(Level, "", Val, [], State),
    emit(Level, "", Message, State).


emit(Level, Location, Message, State) ->
    emit(Level, Location, Message, [], State).

emit(Level, Location, Message, Params, #state{url = Url} = State) ->
    Headers = [{"X-Auth", auth_header(State)}],
    Msg = prepare_message(Level, Location, Message, Params, State),
    io:format("~p~n", [binary:bin_to_list(Url)]),
    Request = {binary:bin_to_list(Url), Headers, "application/json", Msg},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, R}         -> R;
        {error, Reason} -> {error, Reason}
    end.

emit(_, _, _, _, _, _, _State, 0) ->
    ok;

emit(Level, Date, Time, LevelStr, Location, Message, #state{retry_interval = RetryInterval, protocol = Protocol} = State, Rt) ->
    case emit(protocol, Protocol, Level, Date, Time, LevelStr, Location, Message, State) of
        ok         -> ok;
        {error, _} -> timer:sleep(RetryInterval * 1000),
                      emit(Level, Date, Time, LevelStr, Location, Message, State, Rt - 1)
    end.

emit(protocol, udp, _Level, Date, Time, LevelStr, Location, Message, #state{socket = Socket} = State) ->
    M = prepare_message(LevelStr, Date, Time, Location, Message, State),
    Msg = io_lib:format("~s\n\n~s", [auth_header(State), M]),
    case gen_udp:send(Socket, State#state.hostname, 31337, Msg) of
        {error, Reason} -> io:format("Error fink sending: Reason: ~w State: ~p~n", [Reason, State]),
                           {error, Reason};
        _               -> ok
    end;

emit(protocol, _, _Level, Date, Time, LevelStr, Location, Message, #state{url = Url} = State) ->
    Headers = [{"X-Auth", auth_header(State)}],
    Msg = prepare_message(LevelStr, Date, Time, Location, Message, State),
    Request = {Url, Headers, "application/json", Msg},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, _}         -> ok;
        {error, Reason} -> {error, Reason}
    end.

auth_header(#state{public_key = PublicKey, secret_key = SecretKey}) ->
    io_lib:format(
      "Crashdump fink_ts=~p, fink_client=~s, fink_pkey=~s, fink_skey=~s",
      [calendar:datetime_to_gregorian_seconds(calendar:universal_time()), ?CLIENT, PublicKey, SecretKey]
    ).



connect({udp, #state{port = Port} = State}) ->
    case gen_udp:open(Port, [binary, {active, true}]) of
        {ok, Socket}        -> State#state{socket = Socket};
        {error, eaddrinuse} -> connect({udp, State#state{port = Port + 1}});
        {error, Reason}     -> io:format("Error fink connect using udp: Reason: ~p State: ~p~n", [Reason, State]),
                               connect({http, State#state{protocol = https}})
    end;
connect({http, #state{project = Project} = State}) ->
    State#state{url = list_to_binary(io_lib:format("http://~s/api/~s/push", [State#state.hostname, Project]))};
connect({https, #state{project = Project} = State}) ->
    State#state{url = list_to_binary(io_lib:format("https://~s/api/~s/push", [State#state.hostname, Project]))}.

disconnect({udp, #state{socket = Socket}}) ->
    case Socket of
        undefined -> ok;
        Socket1 -> gen_udp:close(Socket1), ok
    end;
disconnect({_, _State}) ->
    ok.

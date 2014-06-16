-module(lager_fink_backend).
-behaviour(gen_event).
-define(SERVER, ?MODULE).
-define(CLIENT, "lager_fink_backend/0.0.1").
%-define(HOST, "base.api.crashkeeper.com").
-define(HOST, "localhost").


-record(state, {identity,
                level,
                retry_interval,
                retry_times,
                protocol,
                public_key,
                secret_key,
                project,
                port = 31338,
                socket = undefined,
                url = undefined}).



%% ------------------------------------------------------------------
%% Includes
%% ------------------------------------------------------------------

-include_lib("lager/include/lager.hrl").


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([emit/8]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([prepare_message/0]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = #state{identity = proplists:get_value(id, Args),
                   level = lager_util:level_to_num(proplists:get_value(level, Args, info)),
                   retry_interval = proplists:get_value(retry_interval, Args, 5),
                   retry_times = proplists:get_value(retry_times, Args, 5),
                   protocol = proplists:get_value(protocol, Args, http),
                   public_key = proplists:get_value(public_key, Args),
                   secret_key = proplists:get_value(secret_key, Args),
                   project = proplists:get_value(project, Args),
                   port = proplists:get_value(port, Args, 31338)},
    State1 = connect({State#state.protocol, State}),
    {ok, State1}.

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{protocol = Protocol} = State) ->
    disconnect({Protocol, State}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_event({log, Level, {Date, Time}, [LevelStr, Location, Message]},
             #state{retry_times = RetryTimes} = State)
  when Level =< State#state.level ->
    spawn(?MODULE, emit, [Level, Date, Time, LevelStr, Location, Message, State, RetryTimes]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

connect({udp, #state{port = Port} = State}) ->
    case gen_udp:open(Port,
                      [binary, {active, true}]) of
        {ok, Socket} ->
            State#state{socket = Socket};
        {error,eaddrinuse} ->
            connect({udp, State#state{port = Port + 1}});
        {error, Reason} ->
            io:format("Error fink connect using udp: Reason: ~w State: ~p~n", [Reason, State]),
            connect({http, State#state{protocol = http}})
    end;

connect({http, #state{public_key = PublicKey, secret_key = SecretKey, project = Project} = State}) ->
    State#state{url = io_lib:format("http://~s:~s@~s/~s", [PublicKey, SecretKey, ?HOST, Project])};
connect({https, #state{public_key = PublicKey, secret_key = SecretKey, project = Project} = State}) ->
    State#state{url = io_lib:format("https://~s:~s@~s/~s", [PublicKey, SecretKey, ?HOST, Project])}.

disconnect({udp, #state{socket = Socket} = _State}) ->
    case Socket of
        Socket1   -> gen_udp:close(Socket1), ok;
        _ -> ok
    end;
disconnect({_, _State}) ->
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
      "Crashkeeper fink_ts=~p, fink_client=~s, fink_pkey=~s, fink_skey=~s",
      [calendar:datetime_to_gregorian_seconds(calendar:universal_time()), ?CLIENT, PublicKey, SecretKey]
    ).

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

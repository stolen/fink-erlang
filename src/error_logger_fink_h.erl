-module(error_logger_fink_h).
-behaviour(gen_event).
-define(CLIENT, "error_logger_fink_h/0.0.1").
%-define(HOST, "base.api.crashkeeper.com").
-define(HOST, "localhost").

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add_handler/0, add_handler/1]).

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
                url = undefined,

                error = error,
                warning_error = error,
                info_msg = info,
                warning_msg = warn,
                error_report = error,
                info_report = info,
                warning_report = warn}).


%%======================================
%% gen_event Function Definitions
%%======================================

init(Args) ->
    process_flag(trap_exit, true),
    State = #state{},
    State1 = connect({State#state.protocol, State}),
    {ok, State1}.


handle_event({error, _GLeader, {_PID, Msg, Data}}, #state{error = L} = State) ->
    R = ?MODULE:emit(L, Msg, Data),
    {R, State};
handle_event({info_msg, _GLeader, {_PID, Msg, Data}}, #state{info_msg = L} = State) ->
    R = ?MODULE:emit(L, Msg, Data),
    {R, State};
handle_event({warning_msg, _GLeader, {_PID, Msg, Data}}, #state{warning_msg = L} = State) ->
    R = ?MODULE:emit(L, Msg, Data),
    {R, State};

handle_event({error_report, _GLeader, _}, #state{error_report = L} = State) ->
    {R, State};
handle_event({info_report, _GLeader, _}, #state{info_report = L } = State) ->
    {R, State};
handle_event({warning_report, _GLeader, _}, #state{warning_error = L} = State) ->
    {R, State}.


handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, _File, _Type}) ->
    remove_handler;
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, #state{protocol = Protocol} = State) ->
    disconnect({Protocol, State}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_handler(Args) -> error_logger:add_report_handler(?MODULE, Args).
add_handler()     -> error_logger:add_report_handler(?MODULE, []).
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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
    State#state{url = io_lib:format("http://~s:~s@~s/~s", [PublicKey, SecretKey, ?HOST, Project])};
connect({https, #state{public_key = PublicKey, secret_key = SecretKey, project = Project} = State}) ->
    State#state{url = io_lib:format("https://~s:~s@~s/~s", [PublicKey, SecretKey, ?HOST, Project])}.

disconnect({udp, #state{socket = Socket} = _State}) ->
    case Socket of
        Socket1 -> gen_udp:close(Socket1), ok;
        _ -> ok
    end;
disconnect({_, _State}) ->
    ok.

-module(fink_message).

-export([prepare_message/4,
         prepare_message/5,
         prepare_message/6]).

-include("fink.hrl").


prepare_message(Level, Location, Message, State) ->
    prepare_message(Level, Location, Message, [], State).

prepare_message(Level, Location, Message, Params, State) ->
    Datetime = fink_lib:iso_8601_fmt(erlang:localtime()),
    prepare_message({Level, Datetime, Location, Message, Params, State}).

prepare_message(Level, Date, Time, Location, Message, State) ->
    Datetime = io_lib:format("~s ~s", [Date, Time]),
    prepare_message({Level, Datetime, Location, Message, [], State}).


prepare_message({_Level, _Datetime, _Location, _Messafe, _Params, #state{project = undefined}}) ->
    error_logger:error_msg("Fink error. Project is required\n", []),
    ok;
prepare_message({Level, Datetime, Location, Message, Params, #state{project = Project}}) ->
    %% {ok, Paths} = erl_prim_loader:get_path(),
    %% {Platform, Kernel} = os:type(),
    {ok, Hostname} = inet:gethostname(),
    Extra = [{memory, erlang:memory()},
             {node,   atom_to_binary(erlang:node(), latin1)},
             {pid,    binary:list_to_bin(os:getpid())}] ++ Params,

    Content = [{level,       atom_to_binary(Level, latin1)},
               {platform,    <<"erlang">>},
               %% {paths,       lists:map(fun list_to_binary/1, Paths)},
               %% {module_info, os:module_info()},

               {location,    binary:list_to_bin(Location)},
               {server_name, binary:list_to_bin(Hostname)},
               {version,     binary:list_to_bin(element(1, init:script_id()))},

               %% {os_type,     io_lib:format("~s~s", [Platform, Kernel])},
               {pwd,         binary:list_to_bin(os:getenv("PWD"))},
               {project,     binary:list_to_bin(Project)},
               {timestamp,   binary:list_to_bin(Datetime)},
               {extra,       Extra},
               {http_request, <<"">>}]
        ++ prepare_content(Message),
    % io:format("~p~n", [Content]),
    base64:encode(zlib:compress(jsx:encode(Content))).


prepare_content({stacktrace, Message, Stacktrace}) ->
    [{stacktrace,  Stacktrace},
     {message,     Message}];
prepare_content(Message) when is_binary(Message) ->
    [{message,     Message}];
prepare_content(Message) ->
    [{message,     Message}].

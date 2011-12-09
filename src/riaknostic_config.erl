%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(riaknostic_config).

-export([prepare/0,
         data_directories/0,
         get_app_env/1,
         get_app_env/2,
         get_vm_env/1,
         user/0]).

%% @doc Prepares appropriate configuration so the riaknostic script
%%      can run.
-spec prepare() -> ok | {error, iodata()}.
prepare() ->
    prepare([fun start_lager/0, fun load_app_config/0, fun load_vm_args/0]).

prepare([]) ->
    ok;
prepare([Fun|T]) ->
    case Fun() of
        {error, Reason} ->
            {error, Reason};
        _ ->
            prepare(T)
    end.

%% @doc Determine where Riak is configured to store data. Returns a
%%      list of paths to directories defined by storage backends.
-spec data_directories() -> [ file:filename() ].
data_directories() ->
    KVBackend = get_app_env([riak_kv, storage_backend]),
    SearchBackend = get_app_env([riak_search, storage_backend], merge_index_backend),
    case get_app_env([riak_search, enabled]) of
        true ->
            data_directory(KVBackend) ++ data_directory(SearchBackend);
        _ ->
            data_directory(KVBackend)
    end.

%% @doc Get a key out of the app.config file, or if it doesn't exist,
%%      return the Default. See also get_app_env/2.
-spec get_app_env([atom()], term()) -> term().
get_app_env(Keys, Default) ->
    case get_app_env(Keys) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc Get a key out of the app.config file.
-spec get_app_env([atom()]) -> undefined | term().
get_app_env(Keys) ->
    {ok, Env} = application:get_env(riaknostic, app_config),
    find_nested_key(Keys, Env).

%% @doc Get an -env flag out of the vm.args file.
-spec get_vm_env(string()) -> string() | undefined.
get_vm_env(Key) ->
    case application:get_env(riaknostic, vm_env) of
        undefined ->
            undefined;
        {ok, PList} ->
            proplists:get_value(Key, PList)
    end.

%% @doc Determines the user/uid that the installed Riak runs as.
-spec user() -> string().
user() ->
    application:get_env(riaknostic, user).

%% Private functions

start_lager() ->
    application:load(lager),
    case application:get_env(riaknostic, log_level) of
        undefined ->
            {error, "Log level not set!"};
        {ok, Level} ->
            application:set_env(lager, crash_log, undefined),
            %% application:set_env(lager, error_logger_redirect, false),
            application:set_env(lager, handlers, [{lager_console_backend, Level}]),
            lager:start()
    end.

load_app_config() ->
    case application:get_env(riaknostic, etc) of
        undefined ->
            {error, "Cannot find Riak configuration directory!"};
        {ok, Path} ->
            AppConfig = filename:join([Path, "app.config"]),
            case file:consult(AppConfig) of
                {ok, [Config]} ->
                    application:set_env(riaknostic, app_config, Config);
                _ ->
                    {error, io_lib:format("Riak config file ~s is malformed!", [AppConfig])}
            end
    end.

load_vm_args() ->
    case application:get_env(riaknostic, etc) of
        undefined ->
            {error, "Cannot find Riak configuration directory!"};
        {ok, Path} ->
            VmArgs = filename:join([Path, "vm.args"]),
            case file:read_file(VmArgs) of
                {error, Reason} ->
                    {error, io_lib:format("Could not read ~s, received error ~w!", [VmArgs, Reason])};
                {ok, Binary} ->
                    load_vm_args(Binary)
            end
    end.

load_vm_args(Bin) when is_binary(Bin) ->
    load_vm_args(re:split(Bin, "\s*\r?\n\s*", [{return, list}, trim]));
load_vm_args([]) ->
    ok;
load_vm_args([[$#|_]|T]) ->
    load_vm_args(T);
load_vm_args([""|T]) ->
    load_vm_args(T);
load_vm_args(["-name " ++ NodeName|T]) ->
    application:set_env(riaknostic, node_name, string:strip(NodeName)),
    load_vm_args(T);
load_vm_args(["-setcookie " ++ Cookie|T]) ->
    application:set_env(riaknostic, cookie, string:strip(Cookie)),
    load_vm_args(T);
load_vm_args(["-env " ++ Env|T]) ->
    [Key, Value] = re:split(Env, "\s+", [{return, list}, trim]),
    add_or_insert_env(vm_env, {Key, Value}),
    load_vm_args(T);
load_vm_args([[$+|EmuFlags]|T]) ->
    [Flag|Rest] = re:split(EmuFlags, "\s+", [{return,list}, trim]),
    add_or_insert_env(emu_flags, {[$+|Flag], Rest}),
    load_vm_args(T);
load_vm_args([[$-|InitFlags]|T]) ->
    [Flag|Rest] = re:split(InitFlags, "\s+", [{return,list}, trim]),
    add_or_insert_env(init_flags, {[$-|Flag], Rest}),
    load_vm_args(T);
load_vm_args([Line|_]) ->
    {error, io_lib:format("Erroneous line in vm.args: ~s", [Line])}.

add_or_insert_env(Key, Value) ->
    case application:get_env(riaknostic, Key) of
        undefined ->
            application:set_env(riaknostic, Key, [Value]);
        {ok, List} ->
            application:set_env(riaknostic, Key, [Value|List])
    end.

find_nested_key(_, undefined) ->
    undefined;
find_nested_key([], Val) ->
    Val;
find_nested_key([Key|T], PList) ->
    find_nested_key(T, proplists:get_value(Key, PList)).
                    
%% Determine the data directory(ies) for the configured storage backend
-spec data_directory(atom()) -> [ file:filename() ].
data_directory(riak_kv_bitcask_backend) ->
    [ get_app_env([bitcask, data_root]) ];
data_directory(riak_kv_eleveldb_backend) ->
    [ get_app_env([eleveldb, data_root]) ];
data_directory(merge_index_backend) ->
    [ get_app_env([merge_index, data_root]) ];
data_directory(riak_kv_innostore_backend) ->
    [ get_app_env([innostore, data_home_dir]),
      get_app_env([innostore, log_group_home_dir]) ];
data_directory(riak_kv_multi_backend) ->
    [ multi_data_directory(Backend) ||
        Backend <- get_app_env([riak_kv, multi_backend]),
        element(2, Backend) =/= riak_kv_memory_backend ];
data_directory(_) -> %% Memory or unknown backend
    [].

%% Extracts data paths from multi_backend config
multi_data_directory({_, riak_kv_bitcask_backend, Props}) ->
    case proplists:get_value(data_root, Props) of
        undefined ->
            get_app_env([bitcask, data_root]);
        Path when is_list(Path) ->
            Path
    end;
multi_data_directory({_, riak_kv_eleveldb_backend, Props}) ->
    case proplists:get_value(data_root, Props) of
        undefined ->
            get_app_env([eleveldb, data_root]);
        Path when is_list(Path) ->
            Path
    end;
multi_data_directory({_, riak_kv_innostore_backend, Props}) ->
    case proplists:get_value(data_home_dir, Props) of
        undefined ->
            get_app_env([innostore, data_home_dir]);
        Path when is_list(Path) ->
            Path
    end.

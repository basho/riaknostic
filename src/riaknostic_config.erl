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

-export([data_directories/0,
         get_app_env/1,
         get_app_env/2,
         get_vm_env/1,
         user/0]).

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
get_app_env(_) ->
    todo.

%% @doc Get an -env flag out of the vm.args file.
-spec get_vm_env(string()) -> string() | undefined.
get_vm_env(_) ->
    todo.

%% @doc Determines the user/uid that the installed Riak runs as.
-spec user() -> string().
user() ->
    todo.

%% Private functions

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

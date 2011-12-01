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
-module(riaknostic_util).
-compile(export_all).

set_node_name(Name) ->
  case net_kernel:start([Name, longnames]) of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, Reason} ->
      throw({name_error, Reason})
  end.

run_command(Command) ->
  Port = erlang:open_port(
    { 
      spawn,
      Command
    },
    [exit_status, stderr_to_stdout]
  ),

  receive
    {Port, {data, StdOut}} ->
      port_close(Port),
      StdOut
  end.

binary_to_float(Bin) ->
  list_to_float(binary_to_list(Bin)).

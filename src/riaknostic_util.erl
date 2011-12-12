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

-spec short_name(module()) -> iodata() | unicode:charlist().
short_name(Mod) when is_atom(Mod) ->
    re:replace(atom_to_list(Mod), "riaknostic_check_", "", [{return, list}]).

run_command(Command) ->
    lager:debug("Running shell command: ~s", [Command]),
    Port = erlang:open_port({spawn,Command},[exit_status, stderr_to_stdout]),
    receive
        {Port, {data, StdOut}} ->
            lager:debug("Shell command output: ~n~s~n",[StdOut]),
            port_close(Port),
            StdOut
    end.

binary_to_float(Bin) ->
    list_to_float(binary_to_list(Bin)).

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

%% @doc Diagnostic that notes processes which have a high monitor count(>50)
-module(riaknostic_check_monitors).
-behaviour(riaknostic_check).

-export([description/0,
    valid/0,
    check/0,
    format/1]).

-spec description() -> string().
description() ->
    "Note processes with >50 monitors".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    Fun = fun() -> [{Pid,NumMon} || Pid <- processes(),
        [{monitors,Mon}] <- [process_info(Pid, [monitors])],
        NumMon <- [length(Mon)],
        NumMon > 50] end,
    case riaknostic_node:local_command(erlang, apply, [Fun,[]]) of
        [] ->
            [];
        Pids ->
            [{warning, {high_monitor_count, Pids}}]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({high_monitor_count, Pids}) ->
    {"The following processes have more than 50 monitors: ~p", [Pids]}.
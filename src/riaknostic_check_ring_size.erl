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
-module(riaknostic_check_ring_size).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

description() ->
    "Ring size valid".

valid() ->
    riaknostic_node:can_connect().

check() ->
    Stats = riaknostic_node:stats(),
    {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
    {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),

    [ {notice, {ring_size_unequal, RingSize, NumPartitions}} || RingSize /= NumPartitions ].

format({ring_size_unequal, S, P}) ->
    {"The configured ring_creation_size (~B) is not equal to the number of partitions in the ring (~B). "
     "Please verify that the ring_creation_size in app.config is correct.", [S, P]}.

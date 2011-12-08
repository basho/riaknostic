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
-module(riaknostic_check_nodes_connected).
-behaviour(riaknostic_check).

-export([valid/0,
         check/0,
         format/1]).

valid() ->
    riaknostic_node:can_connect().

check() ->
    Stats = riaknostic_node:stats(),
    {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
    {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
    {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

    [ {warning, {node_disconnected, N}} || N <- RingMembers,
                                           N =/= NodeName,
                                           lists:member(N, ConnectedNodes) == false].

format({node_disconnected, Node}) ->
    {"Cluster member ~s is not connected to this node. Please check whether it is down.", [Node]}.

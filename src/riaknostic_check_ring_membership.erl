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
-module(riaknostic_check_ring_membership).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

description() ->
    "Cluster membership validity".
    
valid() ->
    riaknostic_node:can_connect().

check() ->
    Stats = riaknostic_node:stats(),
    {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
    {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),
    case lists:member(NodeName, RingMembers) of
        true ->
            [];
        false ->
            [{warning, {not_ring_member, NodeName}}]
    end.

format({not_ring_member, Nodename}) ->
    {"Local node ~w is not a member of the ring. Please check that the -name setting in vm.args is correct.", [Nodename]}.

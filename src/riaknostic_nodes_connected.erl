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
-module(riaknostic_nodes_connected).
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),

  {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  lager:info(
    "Checking connected nodes: ~p",
    [[CN || CN <- ConnectedNodes, CN =/= node()]]
  ),

  lists:foreach(fun(Node) ->
    case lists:member(Node, ConnectedNodes) of
      true ->
        lager:info("Node is connected to ~s", [Node]);
      false when Node =:= NodeName ->
        ok;
      false ->
        lager:error(
          "Node ~s is part of the ring but not connected to this node.",
          [Node]
        )
    end
  end, RingMembers).

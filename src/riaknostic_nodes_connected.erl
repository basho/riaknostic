-module(riaknostic_nodes_connected).
-export([handle_command/1]).

handle_command(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  
  {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),
  io:format("Checking connected nodes: ~p~n", [[CN || CN <- ConnectedNodes, CN =/= node()]]),
  AllNodesInRingConnected = lists:all(fun(Node) ->
    case lists:member(Node, ConnectedNodes) of
      true ->
        true;
      false when Node =:= NodeName ->
        true;
      false ->
        io:format("Node ~s is part of the ring but not connected to this
          node.", [Node]),
          false
    end
  end, RingMembers),
  
  io:format("All nodes in ring connected: ~p~n",[AllNodesInRingConnected]),
  ok.

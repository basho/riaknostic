-module(riaknostic_nodes_connected).
-export([handle_command/1]).

handle_command(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),
  io:format("Checking connected nodes~n"),
  lists:foreach(fun(Node) ->
    case lists:member(Node, ConnectedNodes) of
      true -> ok;
      false when Node =:= NodeName -> ok;
      false -> io:format("Node ~s is part of the ring but not connected to this node.", [Node])
    end
  end, RingMembers),
  ok.

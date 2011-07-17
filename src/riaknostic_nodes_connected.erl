-module(riaknostic_nodes_connected).
-export([run/2]).

run(Config, Log) ->
  Stats = dict:fetch(riak_stats, Config),

  {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  Log({
    info,
    "Checking connected nodes: ~p",
    [[CN || CN <- ConnectedNodes, CN =/= node()]]
  }),

  lists:foreach(fun(Node) ->
    case lists:member(Node, ConnectedNodes) of
      true ->
        Log({info, "Node is connected to ~s", [Node]});
      false when Node =:= NodeName ->
        ok;
      false ->
        Log({
          error,
          "Node ~s is part of the ring but not connected to this node.",
          [Node]
        })
    end
  end, RingMembers).

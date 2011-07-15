-module(riaknostic_nodes_connected).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),

  {connected_nodes, ConnectedNodes} = lists:keyfind(connected_nodes, 1, Stats),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  [
    {
      info,
      io_lib:format(
        "Checking connected nodes: ~p",
        [[CN || CN <- ConnectedNodes, CN =/= node()]]
      )
    } |
    lists:foldl(fun(Node, Acc) ->
      case lists:member(Node, ConnectedNodes) of
        true ->
          Acc ++ [{
            info,
            io_lib:format("Node is connected to ~s", [Node])
          }];
        false when Node =:= NodeName ->
          Acc;
        false ->
          Acc ++ [{
            error,
            io_lib:format(
              "Node ~s is part of the ring but not connected to this node.",
              [Node]
            )
          }]
      end
    end, [], RingMembers)
  ].

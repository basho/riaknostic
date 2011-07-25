-module(riaknostic_ring_membership).
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  case lists:member(NodeName, RingMembers) of
    true ->
      lager:info("Node is a member of the ring");
    false ->
      lager:error("Node is not a member of the ring")
  end.


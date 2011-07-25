-module(riaknostic_ring_membership).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  case lists:member(NodeName, RingMembers) of
    true ->
      lager:info("Node is a member of the ring");
    false ->
      lager:error("Node is not a member of the ring")
  end.


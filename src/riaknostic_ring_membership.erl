-module(riaknostic_ring_membership).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),

  MemberOfRing = case lists:member(NodeName, RingMembers) of
    true ->
      yes;
    false ->
      no
  end,

  io:format("Current node member of the ring? ~s~n", [MemberOfRing]),

  case MemberOfRing of
    yes -> ok;
    no -> [{error, "Node is not a member of the ring"}]
  end.

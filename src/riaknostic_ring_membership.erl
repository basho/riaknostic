-module(riaknostic_ring_membership).
-export([handle_command/1]).

handle_command(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
  {nodename, NodeName} = lists:keyfind(nodename, 1, Stats),
  io:format("Current node member of the ring? ~s~n", [case lists:member(NodeName, RingMembers) of
    true ->
      "yes";
    false ->
      "no"
  end]),
  ok.


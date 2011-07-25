-module(riaknostic_ring_size_partitions).
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),
  {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
  {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),

  case RingSize == NumPartitions of
    true ->
      lager:info("Ring creation size same as number of partitions.");
    false ->
      lager:error(
        "The ring_creation size value (~B) is not equal to the number of partitions (~B)",
        [RingSize, NumPartitions]
      )
  end.

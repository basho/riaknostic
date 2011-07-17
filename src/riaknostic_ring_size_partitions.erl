-module(riaknostic_ring_size_partitions).
-export([run/2]).

run(Config, Log) ->
  Stats = dict:fetch(riak_stats, Config),
  {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
  {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),

  case RingSize == NumPartitions of
    true ->
      Log({info, "Ring creation size same as number of partitions."});
    false ->
      Log({
        warning,
        "The ring_creation size value (~B) is not equal to the number of partitions (~B)",
        [RingSize, NumPartitions]
      })
  end.

-module(riaknostic_ring_size_partitions).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
  {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),
  case RingSize == NumPartitions of
    true ->
      io:format("Ring creation size same as number of partitions? yes~n"),
      ok;
    false ->
      Msg = io_lib:format(
        "The ring_creation size value (~B) is not equal to the number of partitions (~B)~n",
        [RingSize, NumPartitions]
      ),
      io:format(Msg),
      [{warning, Msg}]
  end.

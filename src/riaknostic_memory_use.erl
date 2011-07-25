-module(riaknostic_memory_use).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {mem_total, MemTotal} = lists:keyfind(mem_total, 1, Stats),
  {mem_allocated, MemAllocated} = lists:keyfind(mem_allocated, 1, Stats),

  lager:info(
    "Erlang VM has allocated ~p KB of ~p KB",
    [MemAllocated div 1024, MemTotal div 1024]
  ),

  RiakHome = dict:fetch(riak_home, Config),
  Output = riaknostic_util:run_command(
   "ps -o pmem,rss,command | grep -P 'beam' | grep -P '" ++ RiakHome ++ "'"
  ),
  [_, Percent, RealSize | _] = re:split(Output, "[ ]+"),

  lager:info(
    "The beam process is using ~s% of the total memory and ~s KB real memory.",
    [Percent, RealSize]
  ),

  case riaknostic_util:binary_to_float(Percent) >= 90 of
    false ->
      ok;
    true ->
      lager:warning(warning, "Beam memory usage is at ~s%", [Percent])
  end.



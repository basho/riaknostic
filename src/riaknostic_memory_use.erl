-module(riaknostic_memory_use).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {mem_total, MemTotal} = lists:keyfind(mem_total, 1, Stats),
  {mem_allocated, MemAllocated} = lists:keyfind(mem_allocated, 1, Stats),

  InfoMsg1 = {
    info,
    io_lib:format(
      "Erlang VM has allocated ~p KB of ~p KB",
      [MemAllocated div 1024, MemTotal div 1024]
    )
  },

  RiakHome = dict:fetch(riak_home, Config),
  Output = riaknostic_util:run_command(
   "ps -o pmem,rss,command | grep -P 'beam' | grep -P '" ++ RiakHome ++ "'"
  ),
  [_, Percent, RealSize | _] = re:split(Output, "[ ]+"),

  InfoMsg2 = {
    info,
    io_lib:format(
      "The beam process is using ~s% of the total memory and ~s KB real memory.~n",
      [Percent, RealSize]
    )
  },

  [InfoMsg1, InfoMsg2 | case Percent >= 90 of
    false ->
      [];
    true ->
      [{
        warning,
        io_lib:format("Beam memory usage is at ~w%", [Percent])
      }]
  end].



-module(riaknostic_memory_use).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {mem_total, MemTotal} = lists:keyfind(mem_total, 1, Stats),
  {mem_allocated, MemAllocated} = lists:keyfind(mem_allocated, 1, Stats),

  io:format("Erlang VM has allocated ~p KB of ~p KB~n", [MemAllocated div 1024, MemTotal div 1024]),

  MemUse = check_sys_memory_use(dict:fetch(riak_home, Config)),

  case MemUse >= 90 of
    false ->
      ok;
    true ->
      [{
        warning,
        io_lib:format("Beam memory usage is at ~w%", [MemUse])
      }]
  end.

check_sys_memory_use(RiakHome) ->
  Output = riaknostic_util:run_command(
   "ps -o pmem,rss,command | grep -P 'beam' | grep -P '" ++ RiakHome ++ "'"
  ),
  [_, Percent, RealSize | _] = re:split(Output, "[ ]+"),
  io:format("The beam process is using ~s% of the total memory and ~s KB real memory.~n", [Percent, RealSize]),
  list_to_float(binary_to_list(Percent)).


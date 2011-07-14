-module(riaknostic_memory_use).
-export([handle_command/1]).

handle_command(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {mem_total, MemTotal} = lists:keyfind(mem_total, 1, Stats),
  {mem_allocated, MemAllocated} = lists:keyfind(mem_allocated, 1, Stats),

  io:format("Erlang VM has allocated ~p KB of ~p KB~n", [MemAllocated div 1024, MemTotal div 1024]),
  check_sys_memory_use(dict:fetch(riak_home, Config)),

  ok.

check_sys_memory_use(RiakHome) ->
  Port = erlang:open_port(
    {
      spawn,
      "ps -o pmem,rss,command | grep -P 'beam' | grep -P '" ++ RiakHome ++ "'"
    },
    [exit_status, stderr_to_stdout]
  ),

  receive
    {Port, {data, StdOut}} ->
      port_close(Port),
      [_, Percent, RealSize | _] = re:split(StdOut, "[ ]+"),
      io:format("The beam process is using ~s% of the total memory and ~s KB real memory.~n", [Percent, RealSize])
  end.

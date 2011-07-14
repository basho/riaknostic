-module(riaknostic_disk_check).
-export([handle_command/1]).

handle_command(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),

  lists:foreach(fun({Path, Capacity, Usage}) -> 
    io:format(
      "Disk mounted at ~p is ~p% full (~p KB / ~p KB) with noatime ~p~n",
      [Path, Usage, Capacity * Usage * 0.01, Capacity, check_noatime(Path)]
    )
  end, DiskDatum),

  ok.

check_noatime(MountPoint) ->
  Port = erlang:open_port(
    { 
      spawn,
      "mount | grep -P ' on " ++ MountPoint ++ " '"
    },
    [exit_status, stderr_to_stdout]
  ),

  receive
    {Port, {data, StdOut}} ->
      port_close(Port),
      [ Line | _ ] = re:split(StdOut, "\\n"),

      case re:run(Line, "noatime") of
        nomatch -> off;
        _ -> on
      end
  end.

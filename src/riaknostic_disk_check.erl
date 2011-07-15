-module(riaknostic_disk_check).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),

  lists:foreach(fun({Path, Capacity, Usage}) -> 
    io:format(
      "Disk mounted at ~p is ~p% full (~p KB / ~p KB) with noatime ~p~n",
      [Path, Usage, Capacity * Usage * 0.01, Capacity, is_noatime(Path)]
    )
  end, DiskDatum),

  ok.

is_noatime(MountPoint) ->
  Ouput = riaknostic_util:run_command("mount | grep -P ' on " ++ MountPoint ++ " '"),
  [ Line | _ ] = re:split(Ouput, "\\n"),
  case re:run(Line, "noatime") of
    nomatch -> off;
    _ -> on
  end.

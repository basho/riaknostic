-module(riaknostic_disk_check).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),

  lists:foldl(fun({Path, Capacity, Usage}, Acc) ->
    Noatime = is_noatime(Path),

    InfoMsg = {info, io_lib:format(
      "Disk mounted at ~p is ~p% full (~p KB / ~p KB) with noatime ~p",
      [Path, Usage, Capacity * Usage * 0.01, Capacity, Noatime]
    )},

    Acc1 = case Usage >= 90 of
      false ->
        Acc;
      true ->
        [{
          warning,
          io_lib:format("Disk mounted at ~p is ~p% full", [Path, Usage])
        } | Acc]
    end,

    [InfoMsg | case Noatime of
      on ->
        Acc1;
      off ->
        [{
          warning,
          io_lib:format("Disk mounted at ~p has noatime off", [Path])
        } | Acc1]
    end]
  end, [], DiskDatum).

is_noatime(MountPoint) ->
  Ouput = riaknostic_util:run_command("mount | grep -P ' on " ++ MountPoint ++ " '"),
  [ Line | _ ] = re:split(Ouput, "\\n"),
  case re:run(Line, "noatime") of
    nomatch -> off;
    _ -> on
  end.

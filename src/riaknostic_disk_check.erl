-module(riaknostic_disk_check).
-export([run/1]).

run(Config) ->
  Stats = dict:fetch(riak_stats, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),

  lists:foreach(fun({Path, Capacity, Usage}) ->
    Noatime = is_noatime(Path),

    lager:info(
      "Disk mounted at ~p is ~p% full (~p KB / ~p KB) with noatime ~p",
      [Path, Usage, Capacity * Usage * 0.01, Capacity, Noatime]
    ),

    case Usage >= 90 of
      false ->
        ok;
      true ->
        lager:warning("Disk mounted at ~p is ~p% full", [Path, Usage])
    end,

    case Noatime of
      on ->
        ok;
      off ->
        lager:warning("Disk mounted at ~p has noatime off", [Path])
    end
  end, DiskDatum).

is_noatime(MountPoint) ->
  Ouput = riaknostic_util:run_command("mount | grep -P ' on " ++ MountPoint ++ " '"),
  [ Line | _ ] = re:split(Ouput, "\\n"),
  case re:run(Line, "noatime") of
    nomatch -> off;
    _ -> on
  end.

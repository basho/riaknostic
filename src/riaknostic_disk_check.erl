-module(riaknostic_disk_check).
-export([handle_command/1]).

handle_command(Config) ->
  find_bitcask_data(Config),

  Stats = dict:fetch(riak_stats, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),
  
  lists:foreach(fun({Path, Capacity, Usage}) -> 
    io:format("Disk mounted at ~p is ~p% full (~p KB / ~p KB)~n", [Path, Usage, Capacity * Usage * 0.01, Capacity])
  end, DiskDatum),
  
  ok.

find_bitcask_data(Config) ->
  Node = dict:fetch(riak_node, Config),

  case rpc:call(Node, application, get_env, [bitcask, data_root]) of
    {ok, DataDir} ->
      DataPath = case lists:nth(1, DataDir) /= $/ of
        true -> dict:fetch(riak_home, Config) ++ "/" ++ DataDir;
        false -> DataDir
      end,

      io:format("The bitcask data directory is located in: ~p~n", [ DataPath ]);
    _ -> undefined
  end.

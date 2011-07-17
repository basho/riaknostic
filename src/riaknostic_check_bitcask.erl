-module(riaknostic_check_bitcask).
-export([run/2]).

run(Config, Log) ->
  Node = dict:fetch(riak_node, Config),
  RiakHome = dict:fetch(riak_home, Config),

  case rpc:call(Node, application, get_env, [bitcask, data_root]) of
    {ok, DataDir} ->
      DataPath = case lists:nth(1, DataDir) /= $/ of
        true -> RiakHome ++ "/" ++ DataDir;
        false -> DataDir
      end,

      Log({info, "Found bitcask data directories in: ~p", [DataPath]}),

      case dict:find(bitcask_threshold, Config) of
        error -> ok;
        {ok, ThresholdSize} ->
          true = code:add_path(RiakHome ++ "/lib/bitcask-1.1.6/ebin/"),
          find_bitcask_large_values(DataPath, ThresholdSize, Log)
      end;
    _ -> ok
  end.

find_bitcask_large_values(DataDir, ThresholdSize, Log) ->
  {ok, Dirs} = file:list_dir(DataDir),

  lists:foreach(fun(Dir) ->
    F = fun(K, V, _Acc) ->
      Vsize = size(V),
      case Vsize > ThresholdSize of
        true ->
          Log({
            warning,
            "Bitcask object ~s (~wB) in ~s over threshold ~w",
            [K, Vsize, Dir, ThresholdSize]
          });
        false ->
          ok
      end
    end,

    Ref = bitcask:open(DataDir ++ "/" ++ Dir),
    Log({info, "Checking bitcask directory: ~s", [Dir]}),
    bitcask:fold(Ref, F, []),
    bitcask:close(Ref)
  end, Dirs).

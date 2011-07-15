-module(riaknostic_check_bitcask).
-export([run/1]).

run(Config) ->
  Node = dict:fetch(riak_node, Config),
  RiakHome = dict:fetch(riak_home, Config),

  case rpc:call(Node, application, get_env, [bitcask, data_root]) of
    {ok, DataDir} ->
      DataPath = case lists:nth(1, DataDir) /= $/ of
        true -> RiakHome ++ "/" ++ DataDir;
        false -> DataDir
      end,

      InfoMsg = {info, io_lib:format("Found bitcask data directories in: ~p", [DataPath])},

      [InfoMsg | case dict:find(bitcask_threshold, Config) of
        error -> [];
        {ok, ThresholdSize} ->
          true = code:add_path(RiakHome ++ "/lib/bitcask-1.1.6/ebin/"),
          find_bitcask_large_values(DataPath, ThresholdSize)
      end];
    _ -> ok
  end.

find_bitcask_large_values(DataDir, ThresholdSize) ->
  {ok, Dirs} = file:list_dir(DataDir),

  lists:foldl(fun(Dir, Acc) ->
    F = fun(K, V, Acc1) ->
      Vsize = size(V),
      case Vsize > ThresholdSize of
        true ->
          [{
            warning,
            io_lib:format(
              "Bitcask object ~s (~wB) in ~s over threshold ~w",
              [K, Vsize, Dir, ThresholdSize]
            )
          } | Acc1];
        false ->
          Acc1
      end
    end,

    Ref = bitcask:open(DataDir ++ "/" ++ Dir),
    Messages = [{info, io_lib:format("Checking bitcask directory: ~s", [Dir])}
      | bitcask:fold(Ref, F, [])],
    bitcask:close(Ref),

    lists:append(Messages, Acc)
  end, [], Dirs).

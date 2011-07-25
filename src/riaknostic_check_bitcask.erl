-module(riaknostic_check_bitcask).
-export([run/1]).

run(Config) ->
  {riak_node, Node} = lists:keyfind(riak_node, 1, Config),
  {riak_home, RiakHome} = lists:keyfind(riak_home, 1, Config),

  case rpc:call(Node, application, get_env, [bitcask, data_root]) of
    {ok, DataDir} ->
      DataPath = case lists:nth(1, DataDir) /= $/ of
        true -> RiakHome ++ "/" ++ DataDir;
        false -> DataDir
      end,

      lager:info("Found bitcask data directories in: ~p", [DataPath]),

      case lists:keyfind(bitcask_threshold, 1, Config) of
        false -> ok;
        {bitcask_threshold, ThresholdSize} ->

        find_bitcask_large_values(DataPath, ThresholdSize)
      end;
    _ -> ok
  end.

find_bitcask_large_values(DataDir, ThresholdSize) ->
  {ok, Dirs} = file:list_dir(DataDir),

  lists:foreach(fun(Dir) ->
    F = fun(K, V, _Acc) ->
      Vsize = size(V),
      case Vsize > ThresholdSize of
        true ->
          lager:warning(
            "Bitcask object ~s (~wB) in ~s over threshold ~w",
            [K, Vsize, Dir, ThresholdSize]
          );
        false ->
          ok
      end
    end,

    Ref = bitcask:open(DataDir ++ "/" ++ Dir),
    lager:info("Checking bitcask directory: ~s", [Dir]),
    bitcask:fold(Ref, F, []),
    bitcask:close(Ref)
  end, Dirs).

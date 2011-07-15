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

      io:format("Found bitcask data directories in: ~p~n", [DataPath]),

      case dict:find(bitcask_threshold, Config) of
        error -> ok;
        {ok, ThresholdSize} ->
          true = code:add_path(RiakHome ++ "/lib/bitcask-1.1.6/ebin/"),
          find_bitcask_large_values(DataPath, ThresholdSize)
      end;
    _ -> ok
  end.

find_bitcask_large_values(DataDir, ThresholdSize) ->
  {ok, Dirs} = file:list_dir(DataDir),

  lists:foldl(fun(Dir, Acc) ->
    io:format("---> ~p\n", [Dir]),
    F = fun(K, V, Acc1) ->
      Vsize = size(V),
      case Vsize > ThresholdSize of
        true ->
          io:format("~w\t~120p\n", [Vsize, K]),
          [{
            warning,
            io_lib:format("Bitcask object ~s (~w) over threshold ~w", [K, Vsize, ThresholdSize])
          } | Acc1];
        false ->
          Acc1
      end
    end,

    Ref = bitcask:open(DataDir ++ "/" ++ Dir),
    Messages = bitcask:fold(Ref, F, []),
    bitcask:close(Ref),

    case Messages of
      [] -> Acc;
      _ -> lists:append(Messages, Acc)
    end
  end, [], Dirs).

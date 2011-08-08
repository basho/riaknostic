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

      case proplists:get_value(bitcask_threshold, Config) of
        undefined -> ok;
        ThresholdSize ->
          ThresholdType = proplists:get_value(bitcask_threshold_type,
                                              Config, blob_size),
          find_bitcask_large_values(DataPath, ThresholdSize, ThresholdType)
      end;
    _ -> ok
  end.

find_bitcask_large_values(DataDir, ThresholdSize, ThresholdType)
    when ThresholdType =:= blob_size
      orelse ThresholdType =:= sibling_count
      orelse ThresholdType =:= vclock_length ->
  {ok, Dirs} = file:list_dir(DataDir),

  lager:info("Checking ~s for ~s over ~w",
             [DataDir, ThresholdType, ThresholdSize]),

  lists:foreach(fun(Dir) ->
    F = fun(K, V, _Acc) ->
      Vsize = calc_size(K, V, ThresholdType),
      case Vsize > ThresholdSize of
        true ->
          lager:warning(
            "Bitcask object ~s (~w) in ~s over threshold ~s of ~w",
            [K, Vsize, Dir, ThresholdType, ThresholdSize]
          );
        false ->
          ok
      end
    end,

    Ref = bitcask:open(DataDir ++ "/" ++ Dir),
    lager:info("Checking bitcask directory: ~s", [Dir]),
    bitcask:fold(Ref, F, ok),
    bitcask:close(Ref)
  end, Dirs);
find_bitcask_large_values(_, _, ThresholdType) ->
  throw({invalid_threshold_type, ThresholdType}).

calc_size(_K, V, blob_size) ->
  erlang:size(V);
calc_size(K, V, sibling_count) ->
  calc_size2(K, V, fun(X) -> length(element(4, X)) end);
calc_size(K, V, vclock_length) ->
  calc_size2(K, V, fun(X) -> length(element(5, X)) end).

calc_size2(K, V, Fun) ->
  try case binary_to_term(V) of
    T when is_tuple(T), element(1, T) == r_object,
           size(T) == 7 ->
      Fun(T)
      end
  catch _:_ ->
    case get(warning_hack) of
      undefined ->
        lager:warning("Unknown blob type, key ~p val ~p\n", [K, V]),
        put(warning_hack, true);
      _ ->
        ok
    end,
    0
  end.

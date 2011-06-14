-module(riaknostic).
-export([main/1,
         run/1,
         find_riak/1,
         fetch_riak_stats/1,
         ping_riak/0,
         print_basic_info/1]).

main(Config) ->
  application:load(riaknostic),
  run(Config).

run([]) ->
  {ok, Directories} = application:get_env(riaknostic, modules),
  riaknostic:run(Directories);
run(Directories) ->
  find_riak([{Dir, [Dir ++ "/libexec/releases/", Dir ++ "/releases/"]} || Dir <- Directories]),

  Node = riaknostic:ping_riak(),
  case Node of
    {error, unreachable} ->
      io:format("Can't reach the local Riak instance. Skipping stats.~n"),
      RiakData = [];
    _ ->
      RiakData = riaknostic:fetch_riak_stats(Node)
  end,

  riaknostic:print_basic_info(RiakData),
  application:start(riaknostic, permanent),

  {ok, Modules} = application:get_env(riaknostic, modules),
  Runner = fun(ModuleName) ->
    Module = list_to_atom("riaknostic_" ++ ModuleName),
    Module:handle_command({riak_home, "/usr/lib64/riak"})
  end,

  lists:foreach(Runner, Modules).

find_riak([]) ->
  false;
find_riak([{Dir, ReleaseDirs}|Directories]) ->
  lists:foreach(fun(ReleaseDir) ->
    case filelib:is_file(ReleaseDir ++ "start_erl.data") of
      true ->
        io:format("Found Riak installation in: ~s~n", [ReleaseDir]);
      false ->
        find_riak(Directories)
    end
  end, ReleaseDirs).

fetch_riak_stats(Node) ->
  io:format("Fetching riak data...~n"),
  rpc:call(Node, riak_kv_stat, get_stats, []).
 
ping_riak() ->
  Ping = net_adm:ping('riaksearch@127.0.0.1'),
  case Ping of
    pang ->
      io:format("Couldn't reach the local Riak node.~n"),
      Node = {error, unreachable};
    pong ->
      [Node|_] = nodes()
  end,
  Node.

print_basic_info(RiakData) ->
  lists:foreach(fun(Stat) ->
    case Stat of
      {sys_otp_release, Release} ->
        io:format("Erlang release: ~s~n", [Release]);
      _ ->
        ok
    end
  end, RiakData).

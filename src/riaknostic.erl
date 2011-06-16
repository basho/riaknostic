-module(riaknostic).
-export([main/1,
         run/1,
         find_riak/1,
         find_riak/2,
         find_riak_logs/1,
         find_logs/1,
         fetch_riak_stats/2,
         ping_riak/0,
         print_basic_info/1]).

main(Config) ->
  application:load(riaknostic),
  run(Config).

run([]) ->
  {ok, Directories} = application:get_env(riaknostic, modules),
  riaknostic:run(Directories);
run(Directories) ->
  Config = dict:new(),
  Config1 = find_riak(Config, [{Dir, [Dir ++ "/libexec/releases/", Dir ++ "/releases/"]} || Dir <- Directories]),

  Config2 = find_logs(Config1),

  Node = riaknostic:ping_riak(),
  Config3 = case Node of
    {error, unreachable} ->
      io:format("Can't reach the local Riak instance. Skipping stats.~n"),
      dict:store(riak_stats, [], Config2);
    _ ->
      riaknostic:fetch_riak_stats(Config2, Node)
  end,

  riaknostic:print_basic_info(dict:fetch(riak_stats, Config3)),
  application:start(riaknostic, permanent),

  {ok, Modules} = application:get_env(riaknostic, modules),
  Runner = fun(ModuleName) ->
    Module = list_to_atom("riaknostic_" ++ ModuleName),
    Module:handle_command(Config3)
  end,

  lists:foreach(Runner, Modules).

find_riak([]) ->
  [];
find_riak([{Dir, ReleaseDirs}|Directories]) ->
  case lists:any(fun(ReleaseDir) ->
                   filelib:is_file(ReleaseDir ++ "start_erl.data")
               end, ReleaseDirs) of
    true ->
      io:format("Found Riak installation in: ~s~n", [Dir]),
      Dir;
    false ->
      find_riak(Directories)
  end.
find_riak(Config, Directories) ->
  Dir = find_riak(Directories),
  dict:store(riak_home, Dir, Config).

find_riak_logs([]) ->
  false;
find_riak_logs([Dir|Directories]) ->
  case filelib:is_dir(Dir) of
    true ->
      Dir;
    false ->
      find_riak_logs(Directories)
  end.

find_logs(Config) ->
  RiakHome = dict:fetch(riak_home, Config),
  Directories = [RiakHome ++ "/../log", RiakHome ++ "/log", RiakHome ++ "/libexec/log", "/var/log/riak", "/opt/log/riak"],
  RiakLogs = find_riak_logs(Directories),
  io:format("Found Riak's log files in: ~s~n", [RiakLogs]),
  dict:store(riak_logs, RiakHome, Config).

fetch_riak_stats(Config, Node) ->
  io:format("Fetching riak data...~n"),
  Stats = rpc:call(Node, riak_kv_stat, get_stats, []),
  dict:store(riak_stats, Stats, Config).
 
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

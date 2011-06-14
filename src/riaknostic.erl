-module(riaknostic).
-export([main/1, fetch_riak_stats/1, ping_riak/0, print_basic_info/1]).

main(Config) ->
  application:load(riaknostic),
  {ok, Modules} = application:get_env(riaknostic, modules),

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

  Runner = fun(ModuleName) ->
    Module = list_to_atom("riaknostic_" ++ ModuleName),
    Module:handle_command({riak_home, "/usr/lib64/riak"})
  end,

  lists:foreach(Runner, Modules).

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

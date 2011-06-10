-module(riaknostic).
-export([main/1]).

main(Config) ->
  application:load(riaknostic),

  io:format("Fetching riak data...~n"),
  Ping = net_adm:ping('riaksearch@127.0.0.1'),
  case Ping of
      pang ->
        io:format("Couldn't reach the local Riak node.~n");

      pong ->
        [Node|_] = nodes(),
        RiakData = rpc:call(Node, riak_kv_stat, get_stats, []),
        io:format("~s~n", [RiakData])
  end,
  {ok, Modules} = application:get_env(riaknostic, modules),
  Runner = fun(ModuleName) ->
      Module = list_to_atom("riaknostic_" ++ ModuleName),
      Module:handle_command({riak_home, "/usr/lib64/riak"})
  end,
  lists:foreach(Runner, Modules),
  application:start(riaknostic, permanent).

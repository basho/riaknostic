-module(riaknostic).
-export([main/1,
         run/1
        ]).

-type opt() :: {atom(), any()}.
-type opt_list() :: [opt()].

-type path() :: string().

-type reason() :: any().
-type error() :: {error(), reason()}.

-spec main(list()) -> none().
main(Args) ->
  application:start(riaknostic),
  Opts = riaknostic_opts:parse(Args),
  run(Opts).

-spec run(opt_list()) -> none().
run(Opts) ->
  SearchDirs = proplists:get_value(dirs, Opts, application:get_env(riaknostic, riak_homes)),
  Dir = case find_riak(SearchDirs) of
    {found, RDir} ->
      io:format("Found Riak installation in: ~s~n", [RDir]),
      RDir;
    not_found ->
      throw("Riak not found.")
  end,

  LogDirs = case find_riak_logs(Dir) of
    {found, RLogDirs} ->
      io:format("Found Riak's log files in: ~s~n", [RLogDirs]),
      RLogDirs;
    not_found ->
      throw("Riak logs not found.")
  end,

  Stats = case ping_riak() of
    {error, unreachable} ->
      io:format("Can't reach the local Riak instance. Skipping stats.~n");
    Node ->
      io:format("Fetching riak data...~n"),
      RStats = fetch_riak_stats(Node),
      lists:foreach(fun(Stat) ->
        case Stat of
          {sys_otp_release, Release} ->
            io:format("Erlang release: ~s~n", [Release]);
          _ ->
            ok
        end
      end, RStats),
      RStats
  end,

  Config = dict:from_list([{riak_home, Dir}, {riak_logs, LogDirs}, {riak_stats, Stats}]),

  {ok, Modules} = application:get_env(riaknostic, riaknostics),
  Runner = fun(Module) ->
    Module:handle_command(Config)
  end,

  lists:foreach(Runner, Modules).

-spec find_riak([path()]) -> {found, path()} | not_found.
find_riak([]) ->
  not_found;

find_riak([Dir | Rest]) ->
  case lists:any(fun(ReleaseDir) ->
    filelib:is_file(Dir ++ ReleaseDir ++ "start_erl.data")
  end, ["/libexec/releases/", "/releases/"]) of
    true ->
      {found, Dir};
    false ->
      find_riak(Rest)
  end.

-spec find_riak_logs([path()]) -> {found, [path(),...]} | not_found.
find_riak_logs(RiakDir) ->
  {ok, RiakLogHomes} = application:get_env(riaknostic, riak_log_homes),
  PossibleLogDirs = lists:map(fun(LogDir) ->
    re:replace(LogDir, "\\$riak_home", RiakDir, [{return,list}])
  end, RiakLogHomes),

  LogDirs = lists:filter(fun(PossibleLogDir) ->
     filelib:is_file(PossibleLogDir)
  end, PossibleLogDirs),

  case LogDirs =:= [] of
    true ->
      not_found;
    false ->
      {found, LogDirs}
  end.

-spec fetch_riak_stats(node()) -> any().
fetch_riak_stats(Node) -> 
  rpc:call(Node, riak_kv_stat, get_stats, []).

-spec ping_riak() -> node() | error().
ping_riak() ->
  { ok, [ { NodeSName, _ } | _ ] } = net_adm:names(),
  Node = list_to_atom(NodeSName ++ "@127.0.0.1"),
  case net_adm:ping(Node) of
    pang ->
      {error, unreachable};
    pong ->
      Node
  end.

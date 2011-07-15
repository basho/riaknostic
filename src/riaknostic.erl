-module(riaknostic).
-export([main/1,
         run/1
        ]).

-import(riaknostic_log_server, [log/2, log/3, log_info/2, log_info/3, log_warning/2, print_log/0]).

-type opt() :: {atom(), any()}.
-type opt_list() :: [opt()].

-type path() :: string().

-type reason() :: any().
-type error() :: {error(), reason()}.

-spec main([string()]) -> none().
main(Args) ->
  application:start(riaknostic),
  case net_kernel:start([riaknostic, longnames]) of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, Reason} ->
      throw({name_error, Reason})
  end,
  Opts = riaknostic_opts:parse(Args),
  run(Opts).

-spec run(opt_list()) -> none().
run(Opts) ->
  {ok, DefaultSearchDirs} = application:get_env(riaknostic, riak_homes),
  SearchDirs = proplists:get_value(dirs, Opts, DefaultSearchDirs),
  Dir = case find_riak(SearchDirs) of
    {found, RDir} ->
      log_info(riaknostic_startup, "Found Riak installation in: ~s", [RDir]),
      RDir;
    not_found ->
      throw("Riak not found.")
  end,

  LogDirs = case find_riak_logs(Dir) of
    {found, RLogDirs} ->
      log_info(riaknostic_startup, "Found Riak's log files in: ~s", [RLogDirs]),
      RLogDirs;
    not_found ->
      throw("Riak logs not found.")
  end,

  {Node, Stats} = case ping_riak() of
    {error, unreachable} ->
      log_warning(riaknostic_startup, "Can't reach the local Riak instance. Skipping stats.");
    RNode ->
      RStats = fetch_riak_stats(RNode),
      lists:foreach(fun(Stat) ->
        case Stat of
          {sys_otp_release, Release} ->
            log_info(riaknostic_startup, "Erlang release: ~s", [Release]);
          _ ->
            ok
        end
      end, RStats),
     {RNode, RStats}
  end,

  Config = dict:from_list([ {riak_node, Node},
                            {riak_home, Dir},
                            {riak_logs, LogDirs},
                            {riak_stats, Stats} | Opts ]),

  {ok, Modules} = application:get_env(riaknostic, riaknostics),

  Runner = fun(Module) ->
    case Module:run(Config) of
      [{_Type, _Msg}|_Rest] = Msgs ->
        log(Module, Msgs);
      _ ->
        ok
    end
  end,

  lists:foreach(Runner, Modules),

  print_log().

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

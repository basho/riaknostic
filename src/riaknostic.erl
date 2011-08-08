-module(riaknostic).

-export([main/1,
         run/1]).

-type opt() :: {atom(), any()}.
-type opt_list() :: [opt()].

-type path() :: string().

-type reason() :: any().
-type error() :: {error(), reason()}.

-spec main([string()]) -> none().
main(Args) ->
  riaknostic_util:set_node_name('riaknostic@127.0.0.1'),
  application:load(riaknostic),

  Opts = riaknostic_opts:parse(Args),

  case Opts of
    [{help, true}|_] ->
      print_usage();
    [{list, true}|_] ->
      list_riaknostics();
    _ ->
      application:start(lager),

      RiaknosticsToRun = case OptArgs = proplists:get_value(args, Opts, []) of
        [] ->
          get_riaknostics();
        _ ->
          [riaknostic |
            [OptArgAtom ||
              OptArg <- OptArgs,
              lists:member(OptArgAtom = list_to_atom("riaknostic_" ++ OptArg), get_riaknostics())
            ]
          ]
      end,

      lists:foldl(fun(Mod, Config) ->
        case Result = Mod:run(Config) of
          [{_,_} | _] ->
            Result;
          _ ->
            Config
        end
      end, Opts, RiaknosticsToRun)
  end.

-spec get_riaknostics() -> [atom(),...].
get_riaknostics() ->
  {ok, Modules} = application:get_env(riaknostic, modules),
  [Mod || Mod <- Modules,
    lists:keyfind(run, 1, Mod:module_info(exports)) =:= {run,1}].

-spec print_usage() -> none().
print_usage() ->
  io:format("Usage: riaknostic [-h] [-l] [-dir <riak directory>] [-bitcask_threshold <int> [-bitcask_threshold_type <type>]] [<module,...>]

\t-h                        Show the program options
\t-l                        List available riaknostic modules
\t-dir                      Specify the location of riak
\t-bitcask_threshold        The size in bytes to be considered a large value
\t-bitcask_threshold_type   Check blob_size, sibling_count, or vclock_length
\tmodule                    A diagnostic. By default, all riaknostics are run\n").

-spec list_riaknostics() -> none().
list_riaknostics() ->
  io:format("List of riaknostics:\n"),
  [_Riaknostic | Riaknostics] = get_riaknostics(),
  lists:foreach(fun(Riaknostic) -> 
    Replaced = re:replace(atom_to_list(Riaknostic), "riaknostic_", ""),
    io:format("    ~s~n", [Replaced])
  end, Riaknostics).

-spec run(opt_list()) -> none().
run(Opts) ->
  {ok, DefaultSearchDirs} = application:get_env(riaknostic, riak_homes),
  SearchDirs = proplists:get_value(dirs, Opts, DefaultSearchDirs),
  Dir = case find_riak(SearchDirs) of
    {found, RDir} ->
      lager:info("Found Riak installation in: ~s", [RDir]),
      RDir;
    not_found ->
      lager:error("Riak not found."),
      exit(riak_not_found)
  end,

  add_riak_lib_to_path(Dir),

  LogDirs = case find_riak_logs(Dir) of
    {found, RLogDirs} ->
        lager:info("Found Riak's log files in: ~s", [RLogDirs]),
      RLogDirs;
    not_found ->
      throw("Riak logs not found.")
  end,

  lager:info("Trying to load and parse vm.arg."),
  VmArgs = load_vm_args(Dir),

  {node_name, NodeName} = lists:keyfind(node_name, 1, VmArgs),
  lager:info("The riak node is named \"~s\"", [NodeName]),

  {cookie, Cookie} = lists:keyfind(cookie, 1, VmArgs),
  lager:info("The riak cluster's cookie is \"~s\"", [Cookie]),
  true = erlang:set_cookie(node(), Cookie),

  Stats = case ping_riak(NodeName) of
    {error, unreachable} ->
      lager:warning("Can't reach the local Riak instance. Skipping stats.");
    ok ->
      RStats = fetch_riak_stats(NodeName),

      {sys_otp_release, Release} = lists:keyfind(sys_otp_release, 1, RStats),
      lager:info("Erlang release: ~s", [Release]),

      RStats
  end,

  [ {riak_node, NodeName},
    {riak_home, Dir},
    {riak_logs, LogDirs},
    {riak_stats, Stats} | Opts ].

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

-spec load_vm_args(path()) -> none().
load_vm_args(RiakPath) ->
  {ok, VmArgsHomes} = application:get_env(riaknostic, riak_vm_args_homes),

  case find_vm_args(VmArgsHomes, RiakPath) of
    not_found ->
      {error, "vm.args not found"};
    {found, Path} ->
      {ok, File} = file:read_file(Path),

      {match, [NodeName]} = re:run(File, "-name\s+([^\s\n]+)[\s\n]",
                                   [{capture,all_but_first,binary}]),


      {match, [Cookie]} = re:run(File,
                                 "-setcookie\s+([^\s\n]+)[\s\n]",
                                   [{capture,all_but_first,binary}]),
      [{node_name, binary_to_atom(NodeName, latin1)}, {cookie, binary_to_atom(Cookie, latin1)}]
  end.

-spec find_vm_args([path()], path()) -> {found, path()} | not_found.
find_vm_args([], _) ->
  not_found;

find_vm_args([PosLoc | Rest], RiakPath) ->
  RePosLoc = re:replace(PosLoc, "\\$riak_home", RiakPath, [{return,list}]) ++ "/vm.args",
  case filelib:is_file(RePosLoc) of
    false ->
      find_vm_args(Rest, RiakPath);
    true ->
      {found, RePosLoc}
  end.

-spec fetch_riak_stats(node()) -> any().
fetch_riak_stats(Node) -> 
  rpc:call(Node, riak_kv_stat, get_stats, []).

-spec ping_riak(atom()) -> node() | error().
ping_riak(Node) ->
  case net_adm:ping(Node) of
    pang ->
      {error, unreachable};
    pong ->
      ok
  end.

-spec add_riak_lib_to_path(path()) -> none().
add_riak_lib_to_path(RiakPath) ->
  lists:foreach(fun(EBin) ->
    code:add_path(EBin)
  end, filelib:wildcard(RiakPath ++ "/lib/*/ebin/")).

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
  riaknostic_util:set_node_name('riaknostic@127.0.0.1'),
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

  log_info(riaknostic_startup, "Trying to load and parse vm.arg."),
  VmArgs = load_vm_args(Dir),

  {node_name, NodeName} = lists:keyfind(node_name, 1, VmArgs),
  log_info(riaknostic_startup, "The riak node is named \"~s\"", [NodeName]),

  {cookie, Cookie} = lists:keyfind(cookie, 1, VmArgs),
  log_info(riaknostic_startup, "The riak cluster's cookie is \"~s\"", [Cookie]),
  true = erlang:set_cookie(node(), Cookie),

  Stats = case ping_riak(NodeName) of
    {error, unreachable} ->
      log_warning(riaknostic_startup, "Can't reach the local Riak instance. Skipping stats.");
    ok ->
      RStats = fetch_riak_stats(NodeName),

      {sys_otp_release, Release} = lists:keyfind(sys_otp_release, 1, RStats),
      log_info(riaknostic_startup, "Erlang release: ~s", [Release]),

      RStats
  end,

  Config = dict:from_list([ {riak_node, NodeName},
                            {riak_home, Dir},
                            {riak_logs, LogDirs},
                            {riak_stats, Stats} | Opts ]),

  {ok, Modules} = application:get_env(riaknostic, riaknostics),

  Runner = fun(Module) ->
    Logger = fun
      ({MsgType, Msg, Data}) ->
        log(Module, {MsgType, Msg}, Data);
      ({MsgType, Msg}) ->
        log(Module, {MsgType, Msg})
    end,
    Module:run(Config, Logger)
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

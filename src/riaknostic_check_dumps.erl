-module(riaknostic_check_dumps).
-export([run/2]).

run(Config, Log) ->
  RiakLogs = dict:fetch(riak_logs, Config),
  case filelib:is_file(RiakLogs ++ "/erl_crash.dump") of
    true ->
      Log({warning, "Crash dump present at ~s/erl_crash.dump", [RiakLogs]});
    false ->
      Log({info, "No crash dump present."})
  end.

-module(riaknostic_check_dumps).
-export([run/1]).

run(Config) ->
  RiakLogs = dict:fetch(riak_logs, Config),
  case filelib:is_file(RiakLogs ++ "/erl_crash.dump") of
    true ->
      lager:warning("Crash dump present at ~s/erl_crash.dump", [RiakLogs]);
    false ->
      lager:info("No crash dump present.")
  end.

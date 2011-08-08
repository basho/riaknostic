-module(riaknostic_check_dumps).
-export([run/1]).

run(Config) ->
  {riak_logs, RiakLogs} = lists:keyfind(riak_logs, 1, Config),
  case filelib:is_file(RiakLogs ++ "/erl_crash.dump") of
    true ->
      lager:warning("Crash dump present at ~s/erl_crash.dump", [RiakLogs]);
    false ->
      lager:info("No crash dump present.")
  end.

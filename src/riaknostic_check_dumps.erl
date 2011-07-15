-module(riaknostic_check_dumps).
-export([run/1]).

run(Config) ->
  RiakLogs = dict:fetch(riak_logs, Config),
  io:format("Crash dump file present? ~s~n", [case filelib:is_file(RiakLogs ++ "/erl_crash.dump") of
    true ->
      "yes";
    false ->
      "no"
  end]),
  ok.

-module(riaknostic_check_dumps).
-export([run/1]).

run(Config) ->
  RiakLogs = dict:fetch(riak_logs, Config),
  DumpPresent = case filelib:is_file(RiakLogs ++ "/erl_crash.dump") of
    true ->
      yes;
    false ->
      no
  end,

  io:format("Crash dump file present? ~s~n", [DumpPresent]),

  case DumpPresent of
    no ->
      ok;
    yes ->
      {
        warning,
        io_lib:format("Crash dump present at ~s/erl_crash.dump", [RiakLogs])
      }
  end.

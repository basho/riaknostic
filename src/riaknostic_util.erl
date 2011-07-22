-module(riaknostic_util).
-compile(export_all).

set_node_name(Name) ->
  case net_kernel:start([Name, longnames]) of
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, Reason} ->
      throw({name_error, Reason})
  end.

run_command(Command) ->
  Port = erlang:open_port(
    { 
      spawn,
      Command
    },
    [exit_status, stderr_to_stdout]
  ),

  receive
    {Port, {data, StdOut}} ->
      port_close(Port),
      StdOut
  end.

binary_to_float(Bin) ->
  list_to_float(binary_to_list(Bin)).

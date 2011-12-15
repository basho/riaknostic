# Riaknostic

Riaknostic performs basic diagnostics on Riak nodes. See the
[Riaknostic homepage](http://riaknostic.basho.com) for user-oriented
information.

## Development

Riaknostic requires a sane GNU build system and a recent version of
Erlang.  It has `lager` and `getopt` as dependencies, so those must be
compatible with your version of Erlang. Release versions are currently
built with R14B03.  See the `rebar.config` file for more details.

To build Riaknostic, simply run `make`:

```bash
$ make
./rebar get-deps
==> riaknostic (get-deps)
Pulling lager from {git,"git://github.com/basho/lager",{branch,"master"}}
Cloning into lager...
Pulling getopt from {git,"git://github.com/jcomellas/getopt.git","2981dfe"}
Cloning into getopt...
==> lager (get-deps)
==> getopt (get-deps)
./rebar compile
==> lager (compile)
Compiled src/lager_util.erl
Compiled src/lager_transform.erl
Compiled src/lager_sup.erl
Compiled src/lager_mochiglobal.erl
Compiled src/lager_stdlib.erl
Compiled src/lager_handler_watcher_sup.erl
Compiled src/lager_handler_watcher.erl
Compiled src/lager_trunc_io.erl
Compiled src/lager_crash_log.erl
Compiled src/lager_file_backend.erl
Compiled src/lager_app.erl
Compiled src/lager.erl
Compiled src/lager_console_backend.erl
Compiled src/lager_format.erl
Compiled src/error_logger_lager_h.erl
==> getopt (compile)
Compiled src/getopt.erl
==> riaknostic (compile)
Compiled src/riaknostic_check.erl
Compiled src/riaknostic_util.erl
Compiled src/riaknostic_node.erl
Compiled src/riaknostic_check_ring_size.erl
Compiled src/riaknostic_check_ring_membership.erl
Compiled src/riaknostic_config.erl
Compiled src/riaknostic_check_memory_use.erl
Compiled src/riaknostic_check_nodes_connected.erl
Compiled src/riaknostic_check_dumps.erl
Compiled src/riaknostic.erl
Compiled src/riaknostic_check_disk.erl
./rebar escriptize
==> lager (escriptize)
==> getopt (escriptize)
==> riaknostic (escriptize)
```

Now you can invoke the script manually via the below command:

```bash
$ ./riaknostic --etc ~/code/riak/rel/riak/etc --base ~/code/riak/rel/riak --user `whoami` [other options]
```

To generate the edoc reference, use `make docs` and then open the
`doc/index.html` file in your browser.  Detailed discussion of the
internal APIs that you can use in developing new diagnostics is found
in the edocs.

## Contributing

1. Fork the project on [Github](https://github.com/basho/riaknostic).
2. Make your changes or additions on a "topic" branch, test and
   document them. If you are making a new diagnostic, make sure you
   give some module-level information about the checks it
   performs. *Note*: diagnostics _should not_ make modifications to
   Riak, only inspect things.
3. Push to your fork and send a pull-request.
4. A Basho Developer Advocate or Engineer will review your
   pull-request and get back to you.

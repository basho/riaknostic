# Riaknostic

Riaknostic performs basic diagnostics on Riak nodes.

## Building

To build Riaknostic execute the following commands

    ./rebar compile
    ./rebar escriptize

This will compile and package Riaknostic into a Erlang shell script.

## Executing

Riaknostic is executed as so:

    ./riaknostic -name <node_name> -setcookie <riak_cluster_cookie> -dir <riak_node_dir>

## TODO

- OPT-ize
- More riaknostics
- Reconsider how riaknostic modules are packaged

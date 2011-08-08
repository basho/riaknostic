# Riaknostic

Riaknostic performs basic diagnostics on Riak nodes.

## Building

To build Riaknostic execute the following command

    ./rebar get-deps compile escriptize

This will compile and package Riaknostic into a Erlang shell script.

## Usage

    riaknostic [-h] [-l] [-dir <riak directory>] [-bitcask_threshold <int> [-bitcask_threshold_type <type>]] [<module,...>]

    -h                        Show the program options
    -l                        List available riaknostic modules
    -dir                      Specify the location of riak
    -bitcask_threshold        The size in bytes to be considered a large value
    -bitcask_threshold_type   Check blob_size, sibling_count, or vclock_length
    module                    A diagnostic. By default, all riaknostics are run

## TODO

- Control over output verbosity
- More riaknostics
- Siblings check

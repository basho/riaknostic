%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc <p>The <code>riaknostic</code> module is the entry point for
%% the escript. It is responsible for parsing command-line arguments
%% and switches, printing the available checks, listing the help text,
%% or running all or the specified checks, depending on the command
%% line.</p>
%%
%% <p>The <code>getopt</code> application and module is used
%% for command-line parsing. The defined switches and arguments are:</p>
%% <pre>$ ./riaknostic --etc etc --base base --user user [-d level] [-l] [-h] [check_name...]</pre>
%%
%% <table class="options">
%% <tr><td><code>--etc etc</code></td><td>the location of the Riak
%%   configuration directory (set automatically by
%%   <code>riak-admin</code>)</td></tr>
%% <tr><td><code>--base base</code></td><td>the base directory of
%%   Riak, aka <code>RUNNER_BASE_DIR</code> (set automatically by
%%   <code>riak-admin</code>)</td></tr>
%% <tr><td><code>--user user</code></td><td>the user that Riak runs as
%%   (set automatically by <code>riak-admin</code>)</td></tr>
%% <tr><td><code>-d, --level level</code>&#160;&#160;</td><td>the severity of
%%   messages you want to see, defaulting to 'notice'. Equivalent to
%%   syslog/<code>lager</code> severity levels.</td></tr>
%% <tr><td><code>-l, --list</code></td><td>lists available checks,
%%   that is, modules that implement <code>riaknostic_check</code>. A
%%   "short name" will be given for ease-of-use.</td></tr>
%% <tr><td><code>-h, --help</code></td><td> - print command usage
%%   ("help")</td></tr>
%% <tr><td><code>check_name</code></td><td>when given, a specific
%%   check or list of checks to run</td></tr>
%% </table>
%% @end
-module(riaknostic).
-export([main/1]).

-define(OPTS, [
               {etc,   undefined, "etc",     string,         undefined                                         },
               {base,  undefined, "base",    string,         undefined                                         },
               {user,  undefined, "user",    string,         undefined                                         },
               {level, $d,        "level",   {atom, notice}, "Minimum message severity level (default: notice)"},
               {list,  $l,        "list",    undefined,      "Describe available diagnostic tasks"             },
               {usage, $h,        "help",    undefined,      "Display help/usage"                              },
               {terms, $m,        undefined, undefined,      "Emit machine readable output"                    },
               {terms, undefined, "mr_out",  string,         "Emit machine readable output to file <mr_out>"   }
              ]).

-define(USAGE_OPTS, [ O || O <- ?OPTS,
                           element(5,O) =/= undefined]).

%% @doc The main entry point for the riaknostic escript.
-spec main(CommandLineArguments::[string()]) -> any().
main(Args) ->
    application:load(riaknostic),

    case getopt:parse(?OPTS, Args) of
        {ok, {Opts, NonOptArgs}} ->
            case process_opts(Opts) of
                list -> list_checks();
                usage -> usage();
                run -> run(NonOptArgs)
            end;
        {error, Error} ->
            io:format("Invalid option sequence given: ~w~n", [Error]),
            usage()
    end.

list_checks() ->
    Descriptions = [ {riaknostic_util:short_name(Mod), Mod:description()} ||
                       Mod <- riaknostic_check:modules() ],
    io:format("Available diagnostic checks:~n~n"),
    lists:foreach(fun({Mod, Desc}) ->
                          io:format("  ~.20s ~s~n", [Mod, Desc])
                  end, lists:sort(Descriptions)).

usage() ->
    getopt:usage(?USAGE_OPTS, "riak-admin diag", "[check_name ...]", [{"check_name", "A specific check to run"}]).

run(InputChecks) ->
    case riaknostic_config:prepare() of
        {error, Reason} ->
            io:format("Fatal error: ~s~n", [Reason]),
            halt(1);
        _ ->
            ok
    end,
    Checks = case InputChecks of
                 [] ->
                     riaknostic_check:modules();
                 _ ->
                     ShortNames = [{riaknostic_util:short_name(Mod), Mod} || Mod <- riaknostic_check:modules() ],
                     element(1, lists:foldr(fun validate_checks/2, {[], ShortNames}, InputChecks))
             end,
    Messages = lists:foldl(fun(Mod, Acc) ->
                                   Acc ++ riaknostic_check:check(Mod)
                           end, [], Checks),
    case Messages of
        [] ->
            halt(0);
        _ ->
            %% Print the most critical messages first
            LogLevelNum = lager:minimum_loglevel(lager:get_loglevels()),
            FilteredMessages = lists:filter(fun({Level,_,_}) ->
                                                    lager_util:level_to_num(Level) =< LogLevelNum
                                            end, Messages),
            SortedMessages = lists:sort(fun({ALevel, _, _}, {BLevel, _, _}) ->
                                                lager_util:level_to_num(ALevel) =< lager_util:level_to_num(BLevel)
                                        end, FilteredMessages),
            case SortedMessages of
                [] ->
                    halt(0);
                _ ->
                    case application:get_env(riaknostic, terms) of
                        {ok, true} ->
                            {ok, Path} = application:get_env(riaknostic, term_target),
                            riaknostic_check:write_mr_list(SortedMessages, Path);
                        undefined ->		    
                            lists:foreach(fun riaknostic_check:print/1, SortedMessages)
                    end,
                    halt(1)
            end
    end.

validate_checks(Check, {Mods, SNames}) ->
    case lists:keyfind(Check, 1, SNames) of
        {Check, Mod} ->
            {[Mod|Mods], lists:delete({Check, Mod}, SNames)};
        _ ->
            lager:warning("Unknown check '~s' specified, skipping.", [Check]),
            {Mods, SNames}
    end.

process_opts(Opts) ->
    process_opts(Opts, run).

process_opts([], Result) ->
    Result;
process_opts([H|T], Result) ->
    process_opts(T, process_option(H, Result)).

process_option({etc,Path}, Result) ->
    application:set_env(riaknostic, etc, filename:absname(Path)),
    Result;
process_option({base, Path}, Result) ->
    application:set_env(riaknostic, base, filename:absname(Path)),
    Result;
process_option({user, User}, Result) ->
    application:set_env(riaknostic, user, User),
    Result;
process_option({level, Level}, Result) ->
    application:set_env(riaknostic, log_level, Level),
    Result;
process_option(list, usage) -> %% Help should have precedence over listing checks
    usage;
process_option(list, _) ->
    list;
process_option(usage, _) ->
    usage;
process_option(terms, Result) ->
    process_option({terms, stdin}, Result);
process_option({terms, Path}, Result) ->
    application:set_env(riaknostic, terms, true),
    application:set_env(riaknostic, term_target, Path),
    Result.

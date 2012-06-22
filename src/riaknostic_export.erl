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

%% @doc Gather and export system stats and other 
%% diagnostic output.
%% @end

-module(riaknostic_export).
-export([export/0]).


%% @doc wrapper for all the moving parts of the export function
%% XXX/evan terrible first draft error-handling
-spec export() -> ok | {error, string()}.
export() ->
    TmpDir = prep_tmp_dir(),
    CmdList = get_cmd_list(),
    Outputs = run_commands(CmdList),
    write_to_file(Outputs, TmpDir),
    cleanup_tmp_dir(TmpDir).

get_cmd_list() ->
    case os:type() of 
        {unix, darwin} ->
            [
             %% the pattern here should be:
             %% {"desired_filename", "command_to_run"},
             {"iostat", "iostat 1 5"}
             %% {"fstab", "cat /etc/fstab"}
            ];
        {unix, linux}  -> 
            [
             {"iostat", "iostat 1 5"},
             {"fstab", "cat /etc/fstab"}
            ];
        _ -> [] % maybe throw an error here?
    end.

run_commands(CmdList) ->
    [{Name, riaknostic_util:run_command(Cmd)} ||
        {Name, Cmd} <- CmdList].

write_to_file([], Dir) -> 
    % gathered everything, now package it;
    {ok, NameList} = file:list_dir(Dir),
    FileList = ["export/" ++ File || File <- NameList],
    PrefLen = string:len(Dir) - string:len("export/"),
    Prefix = string:sub_string(Dir, 1, PrefLen),
    io:format("~s~n", [[FileList, Prefix, Dir]]),
    zip:zip("export.zip", FileList, [{cwd, Prefix}, verbose]);

write_to_file(Outputs, Dir) ->
    [H|T] = Outputs,
    {Filename, Output} = H,
    file:write_file(Dir ++ "/" ++ Filename, Output),
    write_to_file(T, Dir).

prep_tmp_dir() ->
    % this may not be a good idea
    {A, B, C} = now(),
    DirPrefix = "/tmp/" ++ integer_to_list(A+B+C),
    file:make_dir(DirPrefix),
    DirName = DirPrefix ++ "/export/",
    file:make_dir(DirName),
    DirName.

cleanup_tmp_dir(DirName) ->
    {ok, FileNames} = file:list_dir(DirName),
    lists:map(fun file:delete/1, FileNames),
    file:del_dir(DirName).



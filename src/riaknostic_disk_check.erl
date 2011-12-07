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
-module(riaknostic_disk_check).
-behaviour(riaknostic_check).

-include_lib("kernel/include/file.hrl").

-export([valid/1,
         check/1,
         format/2]).

-spec valid(riaknostic:config()) -> true | false.
valid(_Config) ->
    true.

-spec check(riaknostic:config()) -> [{lager:log_level(), term()}].
check(Config) ->
  % This is probably wrong :)
    DataDir = riaknostic_config:get_app_config("DATA_DIR", Config),
    
    
    FileName = filename:join([filename:absname(DataDir), "noatime.tmp"]),
    

    case filelib:is_dir(DataDir) of
        true ->
            case file:write_file(FileName, [""]) of
                ok ->
                    case file:read_file_info(FileName) of
                        {ok, FileInfo1} -> 
                            ATime1 = FileInfo1#file_info.atime,
                            timer:sleep(1001),
                            case file:open(FileName, read) of
                                {ok, S} ->
                                    io:get_line(S, ''),
                                    file:close(S),
                                    case file:read_file_info(FileName) of
                                        {ok, FileInfo2} ->
                                            ATime2 = FileInfo2#file_info.atime,
                                            file:delete(FileName),
                                            case (ATime1 =/= ATime2) of
                                                true ->
                                                    [{error, {atime, DataDir}}];
                                                _ ->
                                                    [] 
                                            end;
                                        _ ->
                                            [{warn, {no_read, FileName}}]
                                    end;
                                _ ->
                                    [{warn, {no_read, FileName}}]
                            end;
                        _ ->
                            [{warn, {no_read, FileName}}]
                    end;   
                {error, _} ->
                    [{error, {no_write, DataDir}}]                    
            end;
        _ ->
            [{error, {no_data_dir, DataDir}}]
    end.

-spec format(term(), riaknostic:config()) -> iolist() | {io:format(), [term()]}.
format({no_data_dir, DataDir}, _Config) ->
    {"Data directory ~s does not exist", [DataDir]};
format({no_write, DataDir}, _Config) ->
    {"No write access to data directory ~s.", [DataDir]};
format({no_read, DataDir}, _Config) ->
    {"No read access to data directory ~s.", [DataDir]};
format({atime, Dir}, _Config) ->
    {"Data directory ~s is not mounted with 'noatime'", [Dir]}.

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
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),
  {disk, DiskDatum} = lists:keyfind(disk, 1, Stats),

  lists:foreach(fun({Path, Capacity, Usage}) ->
    Noatime = is_noatime(Path),

    lager:info(
      "Disk mounted at ~p is ~p% full (~p KB / ~p KB) with noatime ~p",
      [Path, Usage, Capacity * Usage * 0.01, Capacity, Noatime]
    ),

    case Usage >= 90 of
      false ->
        ok;
      true ->
        lager:warning("Disk mounted at ~p is ~p% full", [Path, Usage])
    end,

    case Noatime of
      on ->
        ok;
      off ->
        lager:warning("Disk mounted at ~p has noatime off", [Path])
    end
  end, DiskDatum).

is_noatime(MountPoint) ->
  Ouput = riaknostic_util:run_command("mount | grep -P ' on " ++ MountPoint ++ " '"),
  [ Line | _ ] = re:split(Ouput, "\\n"),
  case re:run(Line, "noatime") of
    nomatch -> off;
    _ -> on
  end.

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
-module(riaknostic_memory_use).
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),
  {mem_total, MemTotal} = lists:keyfind(mem_total, 1, Stats),
  {mem_allocated, MemAllocated} = lists:keyfind(mem_allocated, 1, Stats),

  lager:info(
    "Erlang VM has allocated ~p KB of ~p KB",
    [MemAllocated div 1024, MemTotal div 1024]
  ),

  {riak_home, RiakHome} = lists:keyfind(riak_home, 1, Config),
  Output = riaknostic_util:run_command(
   "ps -o pmem,rss,command | grep -P 'beam' | grep -P '" ++ RiakHome ++ "'"
  ),
  [_, Percent, RealSize | _] = re:split(Output, "[ ]+"),

  lager:info(
    "The beam process is using ~s% of the total memory and ~s KB real memory.",
    [Percent, RealSize]
  ),

  case riaknostic_util:binary_to_float(Percent) >= 90 of
    false ->
      ok;
    true ->
      lager:warning("Beam memory usage is at ~s%", [Percent])
  end.



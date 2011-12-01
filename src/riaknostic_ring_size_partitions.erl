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
-module(riaknostic_ring_size_partitions).
-export([run/1]).

run(Config) ->
  {riak_stats, Stats} = lists:keyfind(riak_stats, 1, Config),
  {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
  {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),

  case RingSize == NumPartitions of
    true ->
      lager:info("Ring creation size same as number of partitions.");
    false ->
      lager:error(
        "The ring_creation size value (~B) is not equal to the number of partitions (~B)",
        [RingSize, NumPartitions]
      )
  end.

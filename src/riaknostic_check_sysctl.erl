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

%% @doc Diagnostic that checks the sysctl settings and offers conservative
%% recommendations 
-module(riaknostic_check_sysctl).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-define(LINUX_PARAMS, [
                       {"vm.swappiness",                        0, gte}, 
                       {"net.core.wmem_default",          8388608, lte},
                       {"net.core.rmem_default",          8388608, lte},
                       {"net.core.wmem_max",              8388608, lte},
                       {"net.core.rmem_max",              8388608, lte},
                       {"net.core.netdev_max_backlog",      10000, lte},
                       {"net.core.somaxconn",                4000, lte},
                       {"net.ipv4.tcp_max_syn_backlog",     40000, lte},
                       {"net.ipv4.tcp_fin_timeout",            15, gte},
                       {"net.ipv4.tcp_tw_reuse",                1, eq}
                      ]).
					  
-spec description() -> string().
description() ->
    "Check sysctl tuning parameters".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    Params = case os:type() of
                 {unix, linux} -> ?LINUX_PARAMS;
                 {unix, darwin} -> []; 
                 {unix, freebsd} -> [];
                 {unix, sunos} -> []
             end,
	
	case find_sysctl() of
		{ok, SysctlPath} ->
			check_params(Params, SysctlPath);
		{error, notfound} -> 
			[{critical,{error, notfound}}]
	end.

find_sysctl() ->
	case filelib:is_file("/sbin/sysctl") of
		true -> {ok, "/sbin/sysctl"};
		false ->
			case filelib:is_file("/usr/sbin/sysctl") of
				true ->
					{ok, "/usr/sbin/sysctl"};
				false ->
					{error, notfound}
			end
	end.
		
check_params(List, SysctlPath) ->
    check_params(List, [], SysctlPath).

check_params([], Acc, _SysctlPath) ->
    Acc;

check_params([H|T], Acc, SysctlPath) ->
    {Param, Val, Direction } = H,
    Output = riaknostic_util:run_command(SysctlPath ++ " -n " ++ Param),
    Actual = list_to_integer(Output -- "\n"),
    Good = case Direction of
               gte -> Actual =< Val;
               lte -> Actual >= Val;
               eq -> Actual == Val
           end,
    Note = case Good of 
               true ->  {info, {good, Param, Actual, Val, Direction}};
               false -> {critical, {bad, Param, Actual, Val, Direction}}
           end,
    check_params(T, Acc ++ [Note], SysctlPath).
                       
-spec format(term()) -> {io:format(), [term()]}.
format({good, Param, Actual, Val, Direction}) ->
    {"~s is ~p ~s ~p", [Param, Actual, direction_to_word(Direction), Val]};
format({bad, Param, Actual, Val, Direction}) ->
    {"~s is ~p, should be ~s~p", [Param, Actual, direction_to_word2(Direction), Val]};
format({error, notfound}) ->
    {"sysctl could not be located in /sbin or /usr/sbin",[]}.

direction_to_word(Direction) ->
    case Direction of 
        gte -> "greater than or equal to";
        lte -> "lesser than or equal to";
        eq  -> "equal to"
    end.

direction_to_word2(Direction) ->
    case Direction of 
        gte -> "no more than ";
        lte -> "at least ";
        eq  -> ""
    end.
             
            

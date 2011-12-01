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
-module(riaknostic_opts).

-export([parse/1]).

parse(Args) ->
    parse(Args, []).

parse([], Options) ->
    Fun = fun(Args) -> lists:reverse(Args) end,
    update(args, Options, [], Fun);

parse(["-h"|_], _Options) ->
    [{help, true}];

parse(["-l"|_], _Options) ->
    [{list, true}];

parse(["-dir", Dir|Args], Options) ->
    Options1 = update(dirs, Options, [], fun(Dirs) -> [Dir|Dirs] end),
    parse(Args, Options1);

parse(["-bitcask_threshold", Size|Args], Options) ->
    Options1 = update(bitcask_threshold, Options, [], fun(_) -> list_to_integer(Size) end),
    parse(Args, Options1);

parse(["-bitcask_threshold_type", Type|Args], Options) ->
    Options1 = update(bitcask_threshold_type, Options, [], fun(_) -> list_to_atom(Type) end),
    parse(Args, Options1);

parse([Arg|Args], Options) ->
    Fun = fun(Others) -> [Arg|Others] end,
    Options1 = update(args, Options, [], Fun),
    parse(Args, Options1).

update(Key, Proplist, Default, Fun) ->
    Value = proplists:get_value(Key, Proplist, Default),
    Value1 = Fun(Value),
    Proplist1 = proplists:delete(Key, Proplist),
    [{Key, Value1}|Proplist1].


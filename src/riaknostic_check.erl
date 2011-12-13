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

%% @doc Enforces a common API among all diagnostic modules and
%% provides some automation around their execution.
-module(riaknostic_check).
-export([behaviour_info/1]).
-export([check/1,
         modules/0,
         print/1]).

%% @doc The behaviour definition for diagnostic modules.
-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{description, 0},
     {valid, 0},
     {check, 0},
     {format, 1}];
behaviour_info(_) ->
    undefined.

%% @doc Runs the diagnostic in the given module, if it is valid. Returns a
%% list of messages that will be printed later using print/1.
-spec check(Module::module()) -> [{lager:log_level(), module(), term()}].
check(Module) ->
    case Module:valid() of
        true ->
            [ {Level, Module, Message} || {Level, Message} <- Module:check() ];
        _ ->
            []
    end.

%% @doc Collects a list of diagnostic modules included in the
%% riaknostic application.
-spec modules() -> [module()].
modules() ->
    {ok, Mods} = application:get_key(riaknostic, modules),
    [ M || M <- Mods,
           Attr <- M:module_info(attributes),
           {behaviour, [?MODULE]} =:= Attr orelse {behavior, [?MODULE]} =:= Attr ].


%% @doc Formats and prints the given message via lager:log/3,4. The diagnostic
%% module's format/1 function will be called to provide a
%% human-readable message. It should return an iolist() or a 2-tuple
%% consisting of a format string and a list of terms.
-spec print({Level::lager:log_level(), Module::module(), Data::term()}) -> ok.
print({Level, Mod, Data}) ->
    case Mod:format(Data) of
        {Format, Terms} ->
            lager:log(Level, self(), Format, Terms);
        String ->
            lager:log(Level, self(), String)
    end.

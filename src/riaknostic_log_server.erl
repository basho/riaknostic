-module(riaknostic_log_server).
-behaviour(gen_server).

% API calls
-export([start_link/0, log/2, log/3, log_info/2, log_info/3, log_warning/2, log_warning/3,
        log_error/2, log_error/3, print_log/0, print_log/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% =====================================================================================
%% log APT calls
%% =====================================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Source, [{_Type, _Msg}|_Rest] = Msgs) ->
  gen_server:cast(?MODULE, {log, {Source, Msgs}});
log(Source, {_Type, _Msg} = MsgTuple) ->
  log(Source, [MsgTuple]).

log(Source, {Type, Msg}, Data) ->
  log(Source, {Type, io_lib:format(Msg, Data)}).

log_info(Source, Msg) ->
  log(Source, {info, Msg}).

log_info(Source, Msg, Data) ->
  log(Source, {info, Msg}, Data).

log_warning(Source, Msg) ->
  log(Source, {warning, Msg}).

log_warning(Source, Msg, Data) ->
  log(Source, {warning, Msg}, Data).

log_error(Source, Msg) ->
  log(Source, {error, Msg}).

log_error(Source, Msg, Data) ->
  log(Source, {error, Msg}, Data).

print_log() ->
  print_log([info, warning, error]).
print_log(WhatTypes) ->
  gen_server:call(?MODULE, {print, WhatTypes}).

%% =====================================================================================
%% gen_server callbacks
%% =====================================================================================

init(_) ->
  { ok, dict:new() }.

%% Begin matching with provided arguments. Responds with new match_id.
handle_call({print, WhatTypes}, _, State) ->
  lists:foreach(fun({Source, MsgList}) ->
    io:format("~n========= ~s =========~n~n", [Source]),

    lists:foreach(fun({Type, Msg}) ->
      case lists:any(fun(T) -> T =:= Type end, WhatTypes) of
        true ->
          io:format("~s: ~s~n", [Type, Msg]);
        false ->
        ok
      end
    end, MsgList)
  end, dict:to_list(State)),
  io:format("~n"),
  {reply, ok, State};

handle_call(Request, _From, State) ->
  io:format("Request: ~p~n", [Request]),
  {reply, ignore, State}.

handle_cast({log, {Source, MsgList}}, State) ->
  State1 = dict:update(
    Source,
    fun(OldMsgList) -> lists:append(OldMsgList, MsgList) end,
    MsgList,
    State
  ),
  {noreply, State1};

handle_cast(Cast, State) ->
  io:format("Handle Cast: ~p~n", [Cast]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Handle Info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

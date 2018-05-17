%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. maj 2018 03:16
%%%-------------------------------------------------------------------
-module(pollution_state_container).
-author("kubi").
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3]).
-export([setState/1, getState/0]).

start_link() ->
  gen_server:start_link(
    {local,pollutionStateContainer},
    pollution_state_container,
    pollution:createMonitor(), []).

init(InitialValue) ->
  {ok, InitialValue}.

setState(Monitor) -> gen_server:call(pollutionStateContainer, {setState, Monitor}).
getState() -> gen_server:call(pollutionStateContainer, getState).

handle_call({setState, NewMonitor}, _From, _) ->
  {reply, NewMonitor, NewMonitor};
handle_call(getState, _From, Monitor) ->
  {reply, Monitor, Monitor}.
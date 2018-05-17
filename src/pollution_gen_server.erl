%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2018 14:18
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("kubi").
-behaviour(gen_server).
-version('1.0').


-export([start_link/0, init/1, handle_call/3, handle_cast/2, crash/0, terminate/2]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMovingMean/3]).


start_link() ->
  gen_server:start_link(
    {local,pollutionServer},
    pollution_gen_server,
    pollution:createMonitor(), []),
  Pid = whereis(pollutionStateContainer),
  case Pid of
    undefined -> spawn_link(fun () -> pollution_state_container:start_link() end);
    _ -> setMonitor(pollution_state_container:getState())
  end,
  {ok, whereis(pollutionServer)}.

init(InitialValue) ->
  {ok, InitialValue}.

setMonitor(Monitor) -> gen_server:call(pollutionServer, {setMonitor, Monitor}).

addStation(StationName, {Longitude, Latitude}) -> gen_server:call(pollutionServer, {addStation, {StationName, {Longitude, Latitude}}}).
addValue(StationOrCoordinates, Data, Type, Value) -> gen_server:call(pollutionServer, {addValue, {StationOrCoordinates, Data, Type, Value}}).
removeValue(StationOrCoordinates, Data, Type) -> gen_server:call(pollutionServer, {removeValue, {StationOrCoordinates, Data, Type}}).
getOneValue(StationOrCoordinates, Type, Data) -> gen_server:call(pollutionServer, {getOneValue, {StationOrCoordinates, Type, Data}}).
getStationMean(Name, Type) -> gen_server:call(pollutionServer, {getStationMean, {Name, Type}}).
getDailyMean(Type, Day) -> gen_server:call(pollutionServer, {getDailyMean, {Type, Day}}).
getMovingMean({Longitude, Latitude}, Type, Day) ->gen_server:call(pollutionServer, {getMovingMean, {{Longitude, Latitude}, Type, Day}}).
crash() -> gen_server:cast(pollutionServer, {crash}).

handle_call({setMonitor, OldMonitor}, _From, _) ->
  {reply, OldMonitor, OldMonitor};
handle_call({addStation, {StationName, {Longitude, Latitude}}}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, StationName, {Longitude, Latitude}),
  {reply, NewMonitor , checkNewMonitor(NewMonitor, Monitor)};
handle_call({addValue, {StationOrCoordinates, Data, Type, Value}}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor, StationOrCoordinates, Data, Type, Value),
  {reply, NewMonitor , checkNewMonitor(NewMonitor, Monitor)};
handle_call({removeValue, {StationOrCoordinates, Data, Type}}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, StationOrCoordinates, Data, Type),
  {reply, NewMonitor , checkNewMonitor(NewMonitor, Monitor)};
handle_call({getOneValue, {StationOrCoordinates, Type, Data}}, _From, Monitor) ->
  Value = pollution:getOneValue(Monitor, StationOrCoordinates, Type, Data),
  {reply, checkValue(Value), Monitor};
handle_call({getStationMean, {Name, Type}}, _From, Monitor) ->
  Value = pollution:getStationMean(Monitor, Name, Type),
  {reply, checkValue(Value), Monitor};
handle_call({getDailyMean, {Type, Day}}, _From, Monitor) ->
  Value = pollution:getDailyMean(Monitor, Type, Day),
  {reply, checkValue(Value), Monitor};
handle_call({getMovingMean, {{Longitude, Latitude}, Type, Day}}, _From, Monitor) ->
  Value = pollution:getMovingMean(Monitor, {Longitude, Latitude}, Type, Day),
  {reply, checkValue(Value), Monitor}.

handle_cast({crash}, Monitor) -> {reply, Monitor}.


checkNewMonitor(NewMonitor, Monitor) ->
  case NewMonitor of
    {error, _} ->  Monitor;
    _ -> NewMonitor
  end.

checkValue(Value) ->
  case Value of
    {error, Msg} -> {error, Msg};
    _ -> Value
  end.

terminate(_Reason, _State) ->
  pollution_state_container:setState(_State),
  ok.
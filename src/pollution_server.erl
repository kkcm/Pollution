%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. maj 2018 00:01
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kubi").

%% API
-export([start/0, stop/0, crash/0, crash1/0]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMovingMean/3]).

start() ->
  register(pollutionServer,spawn_link(fun() -> init() end)).
  %%spawn_link((fun() -> init() end)).

stop() ->
  pollutionServer ! stop.

init() ->
  loop(pollution:createMonitor()).


loop(Monitor) ->
  receive
    stop -> io:format("~p : STOP!~n", [self()]),
      {ok, Monitor};
    {request, Pid, addStation, {StationName, {Longitude, Latitude}}} ->
      NewMonitor = pollution:addStation(Monitor, StationName, {Longitude, Latitude}),
      checkNewMonitor(NewMonitor, Monitor, Pid);
    {request, Pid, addValue, {StationNameOrCoordinates, Date, Type, Value}} ->
      NewMonitor = pollution:addValue(Monitor, StationNameOrCoordinates, Date, Type, Value),
      checkNewMonitor(NewMonitor, Monitor, Pid);
    {request, Pid, removeValue, {StationNameOrCoordinates, Date, Type}} ->
      NewMonitor = pollution:removeValue(Monitor, StationNameOrCoordinates, Date, Type),
      checkNewMonitor(NewMonitor, Monitor, Pid);
    {request, Pid, getOneValue, {StationNameOrCoordinates, Type, Date}} ->
      Value = pollution:getOneValue(Monitor, StationNameOrCoordinates, Type, Date),
      checkValue(Value, Monitor, Pid);
    {request, Pid, getStationMean, {Name, Type}} ->
      Value = pollution:getStationMean(Monitor, Name, Type),
      checkNewMonitor(Value, Monitor, Pid);
    {request, Pid, getDailyMean, {Type, Day}} ->
      Value = pollution:getDailyMean(Monitor, Type, Day),
      checkNewMonitor(Value, Monitor, Pid);
    {request, Pid, getMovingMean, {{Longitude, Latitude}, Type, Day}} ->
      Value = pollution:getMovingMean(Monitor, {Longitude, Latitude}, Type, Day),
      checkNewMonitor(Value, Monitor, Pid);
    crash -> 1/0;
    {request, Pid, crash1, _} -> Pid ! {reply, crash1}, 2/0
  end.

checkNewMonitor(NewMonitor, Monitor, Pid) ->
  case NewMonitor of
    {error, Message} -> Pid ! {reply, Message},
      loop(Monitor);
    _ -> Pid ! {reply, NewMonitor},
      loop(NewMonitor)
  end.

checkValue(Value, Monitor, Pid) ->
  case Value of
    {error, Message} -> Pid ! {reply, Message},
      loop(Monitor);
    _ -> Pid ! {reply, Value},
      loop(Monitor)
  end.

call(Message, Arguments) ->
  pollutionServer ! {request, self(), Message, Arguments},
  receive
    {reply, Reply} -> Reply
  end.

addStation(StationName, {Longitude, Latitude}) -> call(addStation, {StationName, {Longitude, Latitude}}).
addValue(StationNameOrCoordinates, Date, Type, Value) -> call(addValue, {StationNameOrCoordinates, Date, Type, Value}).
removeValue(StationNameOrCoordinates, Date, Type) -> call(removeValue, {StationNameOrCoordinates, Date, Type}).
getOneValue(StationNameOrCoordinates, Type, Date) -> call(getOneValue, {StationNameOrCoordinates, Type, Date}).
getStationMean(Name, Type) -> call(getStationMean, {Name, Type}).
getDailyMean(Type, Day)  -> call(getDailyMean, {Type, Day}).
getMovingMean({Longitude, Latitude}, Type, Day) -> call(getMovingMean, {{Longitude, Latitude}, Type, Day}).
crash() -> pollutionServer ! crash.
crash1() -> call(crash1, {}).


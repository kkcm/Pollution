%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2018 08:53
%%%-------------------------------------------------------------------
-module(pollution).
-author("kubi").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMovingMean/4]).

-record (monitor, {stations = #{}, locations = #{}}).
-record (station, {name = "", coordinates, measurements = #{}}).
-record (measurement, {type, value, date}).
-record (date, {date = {{year, month, day}, {hour, minute, second}}}).

createMonitor() -> #monitor{}.


addStation(Monitor, StationName, {Longitude, Latitude}) ->
  StationNameUsed = exist(findStationName(StationName, Monitor)),
  CoordinatesUsed = exist(findCoordinates({Longitude, Latitude}, Monitor)),
  addStation(StationNameUsed, CoordinatesUsed, Monitor, StationName, {Longitude, Latitude}).

addStation(false, false, Monitor, StationName, {Longitude, Latitude}) ->
  #monitor{
    stations = maps:put(StationName, #station{name = StationName, coordinates = {Longitude, Latitude}}, Monitor#monitor.stations),
    locations = maps:put({Longitude, Latitude}, StationName, Monitor#monitor.locations)};
addStation(true,  false, _,  _, {_, _}) ->
  {error, "Station with this name already exists."};
addStation(false,  true, _,  _, {_, _}) ->
  {error, "Station with this coordinates already exists."};
addStation(_,  _, _,  _, {_, _}) ->
  {error, "Station with this name and coordinates already exists."}.


addValue(Monitor, StationNameOrCoordinates, Date, Type, Value) ->
  case isDateTime(Date) and is_atom(Type) of
    false -> {error, "Date or type is wrong."};
    true -> case StationNameOrCoordinates of
              {Longitude, Latitude} -> addValueChecker(exist(findCoordinates({Longitude, Latitude}, Monitor)), Monitor, {Longitude, Latitude}, Date, Type, Value);
              StationName -> addValueChecker(exist(findStationName(StationName, Monitor)), Monitor, StationName, Date, Type, Value)
            end
  end.

addValueChecker(false, _, _, _, _, _) ->
  {error, "You can't add measurement to not existing station."};
addValueChecker(true, Monitor, Coordinates = {_,_}, Date, Type, Value) ->
  {_, StationName} = findCoordinates(Coordinates, Monitor),
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = exist(findMeasurementInStation(Station, Type, Date)),
  addValueToStation(MeasurementExist, Monitor, Station, Date, Type, Value);
addValueChecker(true, Monitor, StationName, Date, Type, Value) ->
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = exist(findMeasurementInStation(Station, Type, Date)),
  addValueToStation(MeasurementExist, Monitor, Station, Date, Type, Value).

addValueToStation (false, Monitor, Station, Date, Type, Value) ->
  Monitor#monitor{
    stations = maps:put(Station#station.name, Station#station{measurements = maps:put(
      {Type, Date}, #measurement{ type = Type, value = Value, date = Date}, Station#station.measurements
    )}, Monitor#monitor.stations)};
addValueToStation(true, _, __, _, _, _) ->
  {error, "You can't add the same measurement to this station."}.


removeValue(Monitor, StationNameOrCoordinates, Date, Type) ->
  case isDateTime(Date) and is_atom(Type) of
    false -> {error, "Date or type is wrong."};
    true -> case StationNameOrCoordinates of
              {Longitude, Latitude} -> removeValueChecker(exist(findCoordinates({Longitude, Latitude}, Monitor)), Monitor, {Longitude, Latitude}, Date, Type);
              StationName -> removeValueChecker(exist(findStationName(StationName, Monitor)), Monitor, StationName, Date, Type)
            end
  end.

removeValueChecker(false, _, _, _, _) ->
  {error, "You can't remove measurement from not existing station."};
removeValueChecker(true, Monitor, Coordinates = {_,_}, Date, Type) ->
  {_, StationName} = findCoordinates(Coordinates, Monitor),
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = exist(findMeasurementInStation(Station, Type, Date)),
  removeValueFromStation(MeasurementExist, Monitor, Station, Date, Type);
removeValueChecker(true, Monitor, StationName, Date, Type) ->
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = exist(findMeasurementInStation(Station, Type, Date)),
  removeValueFromStation(MeasurementExist, Monitor, Station, Date, Type).

removeValueFromStation (true, Monitor, Station, Date, Type) ->
  Monitor#monitor{
    stations = maps:update(Station#station.name, Station#station{measurements = maps:remove(
      {Type, Date}, maps:remove(Type, Station#station.measurements)
    )}, Monitor#monitor.stations)
  };
removeValueFromStation(false, _, __, _, _) ->
  {error, "You can't remove not existing measurement from this station."}.


getOneValue(Monitor, StationNameOrCoordinates, Type, Date) ->
  case isDateTime(Date) and is_atom(Type) of
    false -> {error, "Date or type is wrong."};
    true -> case StationNameOrCoordinates of
              {Longitude, Latitude} -> getOneValueChecker(exist(findCoordinates({Longitude, Latitude}, Monitor)), Monitor, {Longitude, Latitude}, Date, Type);
              StationName -> getOneValueChecker(exist(findStationName(StationName, Monitor)), Monitor, StationName, Date, Type)
            end
  end.

getOneValueChecker(false, _, _, _, _) ->
  {error, "You can't get one value from measurement from not existing station."};
getOneValueChecker(true, Monitor, Coordinates = {_,_}, Date, Type) ->
  {_, StationName} = findCoordinates(Coordinates, Monitor),
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = findMeasurementInStation(Station, Type, Date),
  getOneValueFromMeasurement(MeasurementExist);
getOneValueChecker(true, Monitor, StationName, Date, Type) ->
  {_, Station} = findStationName(StationName, Monitor),
  MeasurementExist = findMeasurementInStation(Station, Type, Date),
  getOneValueFromMeasurement(MeasurementExist).

getOneValueFromMeasurement(MeasurementExists) ->
  case MeasurementExists of
    {_, Measurement} -> Measurement#measurement.value;
    error -> {error, "You can't get not existing measurement from this station."}
  end.


getStationMean(Monitor, Name, Type) when is_atom(Type) ->
  {_, Station} = findStationName(Name, Monitor),
  {Sum, Counter} = maps:fold(fun ( _, Measurement,  {Sum, Counter}) -> {Sum + Measurement#measurement.value, 1 + Counter} end,
    {0,0},
    maps:filter( fun (_, Measurement) -> Measurement#measurement.type == Type end, Station#station.measurements)),
  safeDivider(Sum, Counter).


getDailyMean(Monitor, Type, Day) when is_atom(Type) ->
  {Sum, Counter} = maps:fold(fun ( _, {StationSum, StationCounter},  {Sum, Counter}) -> {Sum + StationSum, Counter + StationCounter} end,
    {0,0},
    maps:map(fun (_, Station) -> getStationDailyMean(Station, Type, Day) end, Monitor#monitor.stations)),
  safeDivider(Sum, Counter).


getStationDailyMean(Station, Type, Day) ->
  maps:fold(fun ( _, Measurement,  {Sum, Counter}) -> {Sum + Measurement#measurement.value, 1 + Counter} end,
    {0,0},
    maps:filter( fun (_, Measurement) -> (Measurement#measurement.type == Type) and (getDateFromDateTime(Measurement#measurement.date) == Day) end,Station#station.measurements)).

getMovingMean(Monitor, {Longitude, Latitude}, Type, Day ) when is_atom(Type) ->
  {_, StationName} = findCoordinates({Longitude, Latitude}, Monitor),
  {_, Station} = findStationName(StationName, Monitor),
  {Sum, Weight} = maps:fold(fun (_, Measurement, {Sum, Weight}) -> {Sum + Measurement#measurement.value * getWeight(Measurement#measurement.date, Day), Weight + getWeight(Measurement#measurement.date, Day) } end,
    {0,0},
    maps:filter( fun (_, Measurement) -> Measurement#measurement.type == Type end, Station#station.measurements)),
  safeDivider(Sum, Weight).

%% helpers

findStationName(StationName, Monitor) ->
  maps:find(StationName, Monitor#monitor.stations).

findCoordinates({Longitude, Latitude}, Monitor) ->
  maps:find({Longitude, Latitude}, Monitor#monitor.locations).

findMeasurementInStation (Station, Type, Date) ->
  maps:find({Type, Date}, Station#station.measurements).

exist(Result) ->
  case Result of
    {ok, _} -> true;
    error -> false
  end.

getWeight(MeasurementTime, CheckTime) ->
  Difference = calendar:time_difference(MeasurementTime, CheckTime),
  case Difference of
    {0, {Hour, _, _}} -> 24 - Hour;
    {_, {_, _, _}} -> 0
  end.

safeDivider(A, 0) when is_number(A) -> 0;
safeDivider(A, B) when is_number(A), is_number(B) -> A / B.

isDateTime(DateTime) ->
  case DateTime of
    {{_ , _, _},{_, _, _}} -> true;
    _ -> false
  end.

getDateFromDateTime (DateTime) ->
  {Date, {_, _, _}} = DateTime,
  Date.

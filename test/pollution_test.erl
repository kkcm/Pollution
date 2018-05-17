%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2018 12:52
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("kubi").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% API
%%-export([]).

-record (monitor, {stations = #{}, locations = #{}}).
-record (station, {name = "", coordinates, measurements = #{}}).
-record (measurement, {type, value, date}).
-record (date, {date = {{year, month, day}, {hour, minute, second}}}).

createMonitor_Test() ->
  {monitor,#{},#{}} = pollution:createMonitor().

addStation_Test() ->
  M = pollution:createMonitor(),
  S1 = #station{name = "S1", coordinates = {1,1}},
  S2 = #station{name = "S2", coordinates = {1.1, 1.1}},
  S3 = #station{name = "S3", coordinates = {14.12, 51.49}},

  M1 = pollution:addStation(M, "S1", {1,1}),
  ?assertEqual(maps:get("S1", M1#monitor.stations), S1),
  M2 = pollution:addStation(M1, "S2", {1.1,1.1}),
  M3 = pollution:addStation(M2, "S3", {14.12,51.49}),
  ?assertEqual(#monitor{stations = #{"S1" => S1, "S2" => S2, "S3" => S3}, locations = #{{14.12,51.49} => "S3", {1.1,1.1} => "S2", {1,1} => "S1"}}, M3),

  ?assertMatch({error, "Station with this name already exists."}, pollution:addStation(M3, "S3", {1,2})),
  ?assertMatch({error, "Station with this coordinates already exists."}, pollution:addStation(M3, "S5", {1,1})),
  ?assertMatch({error, "Station with this name and coordinates already exists."}, pollution:addStation(M3, "S1", {1,1})).

addValue_Test() ->
  Time = calendar:local_time(),
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addValue(M1, "S1", Time, pm10, 50),
  ?assertEqual({monitor, #{"S1" => {station,"S1", {1,1}, #{{pm10,Time} =>
    {measurement,pm10,50, Time}}}}, #{{1,1} => "S1"}}, M2),

  M3 = pollution:addValue(M1, {1, 1}, Time, pm10, 50),
  ?assertEqual({monitor, #{"S1" => {station,"S1", {1,1}, #{{pm10,Time} =>
  {measurement,pm10,50, Time}}}}, #{{1,1} => "S1"}}, M3),

  ?assertMatch({error, "Date or type is wrong."}, pollution:addValue(M2, "S3", Time, "pm10", 30)),
  ?assertMatch({error, "Date or type is wrong."}, pollution:addValue(M2, "S3", 2018, pm10, 30)),
  ?assertMatch({error, "You can't add measurement to not existing station."}, pollution:addValue(M2, "S4", Time, pm10, 30)),
  ?assertMatch({error, "You can't add the same measurement to this station."}, pollution:addValue(M2, "S1", Time, pm10, 30)).


removeValue_Test() ->
  Time = calendar:local_time(),
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addValue(M1, "S1", Time, pm10, 50),
  M3 = pollution:removeValue(M2, "S1", Time, pm10),
  ?assertEqual({monitor, #{"S1" => {station,"S1", {1,1}, #{}}}, #{{1,1} => "S1"}}, M3),

  M4 = pollution:removeValue(M2, {1, 1}, Time, pm10),
  ?assertEqual({monitor, #{"S1" => {station,"S1", {1,1}, #{}}}, #{{1,1} => "S1"}}, M4),

  ?assertMatch({error, "Date or type is wrong."}, pollution:removeValue(M2, "S3", Time, "pm10")),
  ?assertMatch({error, "Date or type is wrong."}, pollution:removeValue(M2, "S3", 2018, pm10)),
  ?assertMatch({error, "You can't remove measurement from not existing station."}, pollution:removeValue(M2, "S4", Time, pm10)),
  ?assertMatch({error, "You can't remove not existing measurement from this station."}, pollution:removeValue(M3, "S1", Time, pm10)).

getOneValue_Test() ->
  Time = calendar:local_time(),
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addValue(M1, "S1", Time, pm10, 50),
  V1 = pollution:getOneValue(M2, "S1", pm10, Time),
  ?assertEqual(50, V1),

  V2 = pollution:getOneValue(M2, {1, 1}, pm10, Time),
  ?assertEqual(50, V2),

  ?assertMatch({error, "Date or type is wrong."}, pollution:getOneValue(M2, {1, 1}, "pm10", Time)),
  ?assertMatch({error, "Date or type is wrong."}, pollution:getOneValue(M2, {1, 1}, pm10, 2018)),
  ?assertMatch({error, "You can't get one value from measurement from not existing station."}, pollution:getOneValue(M2, "S4", pm10, Time)),
  ?assertMatch({error, "You can't get not existing measurement from this station."}, pollution:getOneValue(M2, "S1", pm12, Time)).

getStationMean_Test() ->
  Time = calendar:local_time(),
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addValue(M1, "S1", Time, pm10, 50),
  M3 = pollution:addValue(M2, "S1", calendar:local_time(), pm2, 250),
  M4 = pollution:addValue(M3, "S1", {{2018,4,24},{0,37,34}}, pm10, 300),
  A1 = pollution:getStationMean(M4, "S1", pm10),
  ?assertEqual(175.0, A1).

getDailyMean_Test() ->
  Time = calendar:local_time(),
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addStation(M1, "S3", {12,1}),
  M3 = pollution:addValue(M2, "S1", Time, pm10, 50),
  M4 = pollution:addValue(M3, "S1", calendar:local_time(), pm2, 250),
  M5 = pollution:addValue(M4, "S3", calendar:local_time(), pm10, 300),
  {Date, {_, _, _}} = calendar:local_time(),
  A1 = pollution:getDailyMean(M5, pm10, Date),
  ?assertEqual(175.0, A1).

getMovingMean_Test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "S1", {1,1}),
  M2 = pollution:addValue(M1, "S1", {{2018,4,4},{0,37,34}}, pm10, 5000000),
  M3 = pollution:addValue(M2, "S1", {{2018,4,24},{2,15,34}}, pm10, 50),
  M4 = pollution:addValue(M3, "S1", {{2018,4,23},{4,15,34}}, pm10, 700),
  MA = pollution:getMovingMean(M4, {1,1}, pm10, {{2018,4,24},{2,30,34}}),
  ?assertEqual(100.0, MA).




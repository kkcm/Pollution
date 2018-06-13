%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. maj 2018 01:24
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("kubi").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% API
%%-export([]).

-record (monitor, {stations = #{}, locations = #{}}).
-record (station, {name = "", coordinates, measurements = #{}}).
-record (measurement, {type, value, date}).
-record (date, {date = {{year, month, day}, {hour, minute, second}}}).

%%znaleźć info o ustawianiu stanów begin i after dla eunit


start_Test() ->
  pollution_server:start(),
  ?assertEqual(lists:filter(fun(X) -> case X of pollutionServer -> true; _ -> false end end, registered()), [pollutionServer]).

addStation_Test() ->
  S1 = #station{name = "S1", coordinates = {1,1}},
  S2 = #station{name = "S2", coordinates = {1.1, 1.1}},
  S3 = #station{name = "S3", coordinates = {14.12, 51.49}},

  M1 = pollution_server:addStation("S1", {1,1}),
  ?assertEqual(M1, {monitor, #{"S1" => {station,"S1",{1,1},#{}}}, #{{1,1} => "S1"}}),

  pollution_server:addStation("S2", {1.1,1.1}),
  M3 = pollution_server:addStation("S3", {14.12,51.49}),
  ?assertEqual(M3, #monitor{stations = #{"S1" => S1, "S2" => S2, "S3" => S3}, locations = #{{14.12,51.49} => "S3", {1.1,1.1} => "S2", {1,1} => "S1"}}),

  ?assertMatch("Station with this name already exists.", pollution_server:addStation("S3", {1,2})),
  ?assertMatch("Station with this coordinates already exists.", pollution_server:addStation("S5", {1,1})),
  ?assertMatch("Station with this name and coordinates already exists.", pollution_server:addStation("S1", {1,1})).

addValue_Test() ->
  Time = calendar:local_time(),
  M2 = pollution_server:addValue("S1", Time, pm10, 50),
  ?assertEqual(M2, {monitor,
    #{"S3" => {station,"S3",{14.12,51.49},#{}},
      "S2" => {station,"S2",{1.1,1.1},#{}},
      "S1" =>
      {station,"S1",
        {1,1},
        #{{pm10,Time} =>
        {measurement,pm10,50,Time}}}},
    #{{14.12,51.49} => "S3",
      {1.1,1.1} => "S2",
      {1,1} => "S1"}}),

  M3 = pollution_server:addValue({1, 1}, Time, pm2, 50),
  ?assertEqual(M3, {monitor,
    #{"S3" => {station,"S3",{14.12,51.49},#{}},
      "S2" => {station,"S2",{1.1,1.1},#{}},
      "S1" =>
      {station,"S1",
        {1,1},
        #{{pm2,Time} =>
        {measurement,pm2,50,Time},
          {pm10,Time} =>
        {measurement,pm10,50,Time}}}},
    #{{14.12,51.49} => "S3",
      {1.1,1.1} => "S2",
      {1,1} => "S1"}}),

  ?assertMatch("Date or type is wrong.", pollution_server:addValue("S3", Time, "pm10", 30)),
  ?assertMatch("Date or type is wrong.", pollution_server:addValue("S3", 2018, pm10, 30)),
  ?assertMatch( "You can't add measurement to not existing station.", pollution_server:addValue("S4", Time, pm10, 30)),
  ?assertMatch( "You can't add the same measurement to this station.", pollution_server:addValue("S1", Time, pm10, 30)).

removeValue_Test() ->
  Time = calendar:local_time(),
  pollution_server:addValue("S1", Time, pm10, 50),
  M3 = pollution_server:removeValue("S1", Time, pm10),
  ?assertEqual(M3, {monitor,
    #{"S3" => {station,"S3",{14.12,51.49},#{}},
      "S2" => {station,"S2",{1.1,1.1},#{}},
      "S1" => {station,"S1",{1,1},#{}}},
    #{{14.12,51.49} => "S3",
      {1.1,1.1} => "S2",
      {1,1} => "S1"}}),

  pollution_server:addValue("S1", Time, pm10, 50),
  M4 = pollution_server:removeValue({1, 1}, Time, pm10),
  ?assertEqual(M4, {monitor,
    #{"S3" => {station,"S3",{14.12,51.49},#{}},
      "S2" => {station,"S2",{1.1,1.1},#{}},
      "S1" => {station,"S1",{1,1},#{}}},
    #{{14.12,51.49} => "S3",
      {1.1,1.1} => "S2",
      {1,1} => "S1"}}),

  ?assertMatch("Date or type is wrong.", pollution_server:removeValue("S3", Time, "pm10")),
  ?assertMatch("Date or type is wrong.", pollution_server:removeValue("S3", 2018, pm10)),
  ?assertMatch("You can't remove measurement from not existing station.", pollution_server:removeValue("S4", Time, pm10)),
  ?assertMatch("You can't remove not existing measurement from this station.", pollution_server:removeValue("S1", Time, pm10)).

getOneValue_Test() ->
  Time = calendar:local_time(),
  pollution_server:addValue("S1", Time, pm10, 50),
  V3 = pollution_server:getOneValue("S1", pm10, Time),
  ?assertEqual(V3, 50),

  V4 = pollution_server:getOneValue({1, 1}, pm10, Time),
  ?assertEqual(V4, 50),

  ?assertMatch("Date or type is wrong.", pollution_server:getOneValue({1, 1}, "pm10", Time)),
  ?assertMatch("Date or type is wrong.", pollution_server:getOneValue({1, 1}, pm10, 2018)),
  ?assertMatch("You can't get one value from measurement from not existing station.", pollution_server:getOneValue("S4", pm10, Time)),
  ?assertMatch("You can't get not existing measurement from this station.", pollution_server:getOneValue("S1", pm12, Time)).

getStationMean_Test() ->
  pollution_server:addValue("S1", calendar:local_time(), pm2, 250),
  pollution_server:addValue("S1", {{2018,4,24},{0,37,34}}, pm10, 300),
  A1 = pollution_server:getStationMean("S1", pm10),
  ?assertEqual(175.0, A1).

getDailyMean_Test() ->
  pollution_server:addValue("S1", calendar:local_time(), pm2, 250),
  pollution_server:addValue("S3", calendar:local_time(), pm10, 300),
  {Date, {_, _, _}} = calendar:local_time(),
  A1 = pollution_server:getDailyMean(pm10, Date),
  ?assertEqual(175.0, A1).

getMovingMean_Test() ->
  pollution_server:addValue("S1", {{2018,4,4},{0,37,34}}, pm10, 5000000),
  pollution_server:addValue("S1", {{2018,4,24},{2,15,34}}, pm10, 50),
  pollution_server:addValue("S1", {{2018,4,23},{4,15,34}}, pm10, 700),
  A = pollution_server:getMovingMean({1,1}, pm10, {{2018,4,24},{2,30,34}}),
  ?assertEqual(100.0, A).
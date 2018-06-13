defmodule PollutionData do
  @moduledoc false

  def importAndLoadDataFromCSV(file) do
    data = file
    |> importLinesFromCSV
    |> Enum.map(fn x -> parseLine(x) end)
    t1 = fn -> addStations(data) end |> :timer.tc |> elem(0)
    t2 = fn -> addValues(data) end |> :timer.tc |> elem(0)
    IO.puts "addStation #{t1} | addValues #{t2}"
  end

  def importLinesFromCSV(file) do
    data = File.read!(file) |> String.split("\r\n", trim: true)
    n = length(data)
    IO.puts "I've just imported #{n} lines."
    data
  end

  def parseLine(line) do
    [day, hour, longitude, lattitude, level] = String.split(line, ",")
    date = day |> String.split("-") |> Enum.reverse |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    time = hour <> ":00" |> String.split(":") |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    lattitude = lattitude |> Float.parse |> elem(0)
    longitude = longitude |> Float.parse |> elem(0)
    level = level |> Integer.parse |> elem(0)
    %{datetime: {date,time}, location: {lattitude, longitude}, pollutionLevel: level}
  end

  def parseParams(day, hour, longitude, lattitude, level) do
    date = day |> String.split("-") |> Enum.reverse |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    time = hour <> ":00" |> String.split(":") |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    lattitude = lattitude |> Float.parse |> elem(0)
    longitude = longitude |> Float.parse |> elem(0)
    level = level |> Integer.parse |> elem(0)
    %{datetime: {date,time}, location: {lattitude, longitude}, pollutionLevel: level}
  end

  def identifyStations(parsed_lines) do
    stations_map = parsed_lines |> Enum.reduce(%{}, fn(%{:location => loc}, acc) -> Map.put(acc, loc, loc) end)
    n = map_size(stations_map)
    IO.puts "There is #{n} unique stations."
    stations_map
  end

  def generateStationName(stations_map) do
    stations_map |> Enum.map(fn {coordinates, {longitude, lattitude}} -> {coordinates, "station_#{longitude}_#{lattitude}"} end)
  end

  def addStations(parsed_lines) do

    :pollution_server_supervisor.start_link()
    parsed_lines
    |> identifyStations
    |> generateStationName
    |> Enum.each(fn {coordinates, name} -> :pollution_gen_server.addStation(name, coordinates) end)

  end

  def addValues(parsed_lines) do
    Enum.each(parsed_lines, fn m  -> :pollution_gen_server.addValue(m.location, m.datetime, PM10, m.pollutionLevel) end)
  end

end

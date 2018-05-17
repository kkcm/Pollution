%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2018 13:21
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("kubi").

%% API
-export([start/0, stop/0]).

start() ->
  register(supervisor,spawn_link(fun() -> init() end)).

init() ->
  process_flag(trap_exit, true),
  loop().

loop() ->
  pollution_server:start(),
  receive
    {'EXIT', _, _} -> io:format("Server dead, restarting.~n"),
      loop();
      stop -> ok
  end.

stop() ->
  supervisor ! stop.


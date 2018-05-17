%%%-------------------------------------------------------------------
%%% @author kubi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. maj 2018 01:25
%%%-------------------------------------------------------------------
-module(pollution_server_supervisor).
-author("kubi").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link()->
  supervisor:start_link({local, pollutionServerSupervisor},
    ?MODULE, []).

init(InitialValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [  {pollutionServer,
      {pollution_gen_server, start_link, InitialValue},
permanent, brutal_kill, worker, [pollution_gen_server]}
]}
}
.


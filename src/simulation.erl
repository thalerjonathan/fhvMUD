-module(simulation).

-export([init/0, simProc/0]).

% -define(SERVER, ?MODULE).

init() ->
  io:fwrite("init sim"),
  spawn_link(?MODULE, simpProc, []).

simProc() ->
  io:fwrite("simPRoc"),
  simProc.
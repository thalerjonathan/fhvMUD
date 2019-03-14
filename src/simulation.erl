-module(simulation).

-export([init/0, simProc/0]).

init() ->
  io:fwrite("init sim"),
  Sid = spawn(?MODULE, simpProc, []),
  io:fwrite("Sid: ~p ~n", [Sid]),
  Sid.

simProc() ->
  io:fwrite("simPRoc"),
  receive 
    {newPlayer} ->
      io:fwrite("New Player"),
      simProc()
  end.

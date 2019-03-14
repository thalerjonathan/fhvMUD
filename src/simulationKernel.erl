-module(simulationKernel).

-export([init/0, simProc/0]).

init() ->
  Pid = spawn(?MODULE, simProc, []),
  io:fwrite("Sid: ~p ~n", [Pid]),
  Pid.

simProc() ->
  io:fwrite("Simulation Kernel running ~n"),
  receive 
    {newPlayer, Player} ->
      io:fwrite("New Player ~p ~n", [Player]),
      simProc()
  end.
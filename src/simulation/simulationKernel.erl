-module(simulationKernel).

-export([init/0, simProc/0]).

init() ->
  Pid = spawn(?MODULE, simProc, []),
  io:fwrite("Sid: ~p ~n", [Pid]),
  Pid.

simProc() ->
  io:fwrite("Simulation Kernel running ~n"),
  receive 
    {newPlayer, Pid, PlayerName} ->
      io:fwrite("New Player ~p ~s ~n", [Pid, PlayerName]),
      simProc()
  end.
-module(simulationKernel).

-export([init/0, simProc/0]).

init() ->
  Pid = spawn(?MODULE, simProc, []),
  io:fwrite("Simulation Kernel: ~p ~n", [Pid]),
  
  Pid.

simProc() ->
  Lobby = loadWorld(),
  runSim(Lobby).

loadWorld() ->
  Lobby = lobby:startLobby(self()),
  Lobby.

runSim(Lobby) ->
  io:fwrite("Simulation Kernel running ~n"),
  receive 
    {newPlayer, Player, PlayerName} ->
      io:fwrite("New Player ~p ~s ~n", [Player, PlayerName]),
      % add player to lobby
      Player ! {enteredRoom, Lobby, "Lobby" },
      Lobby ! {playerEntered, Player, PlayerName},
      runSim(Lobby)
  end.
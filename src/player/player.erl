-module(player).

-export([newPlayer/2, playerProc/2]).

newPlayer(Simulation, PlayerName) ->
  Pid = spawn(?MODULE, playerProc, [Simulation, PlayerName]),
  Simulation ! {newPlayer, Pid, PlayerName},
  Pid.

playerProc(Simulation, PlayerName) ->
  receive 
    {forward} ->
      playerProc(Simulation, PlayerName)
  end.
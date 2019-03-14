-module(player).

-export([newPlayer/1, playerProc/1]).

newPlayer(Simulation) ->
  Player = spawn(?MODULE, playerProc, [Simulation]),
  Simulation ! {newPlayer, Player},
  Player.

playerProc(Simulation) ->
  receive 
    {forward} ->
      playerProc(Simulation)
  end.
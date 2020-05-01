-module(simulation).

-export([new/0, newPlayer/2, removePlayer/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
  Pid = spawn(fun() -> init() end),
  io:fwrite("Simulation: ~p ~n", [Pid]),
  Pid.

newPlayer(Simulation, Player) ->
  Simulation ! { newPlayer, Player }.

removePlayer(Simulation, Player) ->
  Simulation ! { removePlayer, Player }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  Lobby = loadWorld(),
  io:fwrite("Simulation running ~n"),
  process(Lobby).

loadWorld() ->
  Lobby = lobby:new(self()),
  Lobby.

process(Lobby) ->
  receive 
    {newPlayer, Player} ->
      io:fwrite("New Player ~w ~n", [Player]),
      player:enteredRoom(Player, Lobby, "Lobby"),
      lobby:playerEnter(Lobby, Player),
      process(Lobby);

    {removePlayer, Player} ->
      io:fwrite("simulation: removed player ~w... ~n", [Player]),
      process(Lobby)
  end.
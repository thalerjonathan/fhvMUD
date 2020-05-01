-module(player).

-export([new/3, remove/1, enteredRoom/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation, Frontend, PlayerName) ->
  Pid = spawn(fun() -> process(Simulation, Frontend, PlayerName) end),
  simulation:newPlayer(Simulation, Pid),
  Pid.

remove(Player) ->
  Player ! {remove}.

% TODO FIX: the roomname should be known only by the room itself!
enteredRoom(Player, Room, RoomName) ->
  Player ! {enteredRoom, Room, RoomName}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(Simulation, Frontend, PlayerName) ->
  receive 
    {remove} ->
      io:fwrite("player: removed ~w ~n", [PlayerName]);
    {enteredRoom, Room, RoomName } ->
      playerFrontend:sendText(Frontend, "Entered Room ~w ~s ~n", [Room, RoomName]);
    Msg ->
      io:fwrite("Received unknown message in player: ~w ~n", [Msg]),
      process(Simulation, Frontend, PlayerName)
  end.
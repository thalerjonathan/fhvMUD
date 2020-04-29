-module(player).

-export([newPlayer/3, playerProc/3]).

newPlayer(Simulation, PlayerFrontend, PlayerName) ->
  Pid = spawn(?MODULE, playerProc, [Simulation, PlayerFrontend, PlayerName]),
  Simulation ! {newPlayer, Pid, PlayerName},
  Pid.

playerProc(Simulation, PlayerFrontend, PlayerName) ->
  receive 
    {forward} ->
      playerProc(Simulation, PlayerFrontend, PlayerName);
    {enteredRoom, RoomPid, RoomName } ->
      PlayerFrontend ! {sendText, io_lib:format("Entered Room ~s ~n", [RoomName])}
  end.
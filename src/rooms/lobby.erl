-module(lobby).

-export([startLobby/1, lobbyProc/1]).

startLobby(Simulation) ->
  Pid = spawn(?MODULE, lobbyProc, [Simulation]),
  Pid.

lobbyProc(Simulation) ->
  receive
    Msg ->
      io:fwrite("Lobby: received message ~w ~n", [Msg]),
      lobbyProc(Simulation)  
  end.

-module(playerFrontend).

-export([spawnPlayer/2, playerSocketProc/3]).

spawnPlayer(Simulation, Socket) -> 
  Player = player:newPlayer(Simulation),
  % spawn a new client-process
  Pid = spawn(?MODULE, playerSocketProc, [Simulation, Player, Socket]),
  % give ownership of the socket to the client process
  gen_tcp:controlling_process(Socket, Pid),
  Pid.

playerSocketProc(Simulation, Player, Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
        gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
        gen_tcp:send(Socket, Msg),
        io:fwrite("Message: ~s ~n", [Msg]),
        playerSocketProc(Simulation, Player, Socket)
  end.

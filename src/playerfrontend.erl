-module(playerfrontend).

-export([newPlayer/1, playerSocketProc/1]).

newPlayer(Socket) -> 
  % spawn a new client-process
  Pid = spawn_link(?MODULE, playerSocketProc, [Socket]),
  % give ownership of the socket to the client process
  gen_tcp:controlling_process(Socket, Pid),
  Pid.

playerSocketProc(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
        gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
        gen_tcp:send(Socket, Msg),
        io:fwrite("Message: ~s ~n", [Msg]),
        playerSocketProc(Socket)
  end.

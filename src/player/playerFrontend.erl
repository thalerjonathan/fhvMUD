-module(playerFrontend).

-export([spawnPlayer/2, playerSocketProc/2]).

spawnPlayer(Simulation, Socket) -> 
  % spawn a new client-process
  Pid = spawn(?MODULE, playerSocketProc, [Simulation, Socket]),
  % give ownership of the socket to the client process
  Ret = gen_tcp:controlling_process(Socket, Pid),
  io:fwrite("controlling_process: ~w ~n", [Ret]),
  Pid.

playerSocketProc(Simulation, Socket) ->
  % send welcome message
  
  Ret = queryName(Socket),
  case Ret of
    {error, quit} ->
      gen_tcp:close(Socket),
      io:fwrite("Player ~w quit during naming ~n", [self()]);
    {error, timeout} ->
      gen_tcp:send(Socket, "\nTimeout, disconnecting...\n"),
      gen_tcp:close(Socket),
      io:fwrite("Timeout, disconnected player ~w ~n", [self()]);
    {ok, PlayerName} ->
      Msg = io_lib:format("Hello ~s! Welcome to Chimera Multi-User Simulation!\n\n", [PlayerName]),
      gen_tcp:send(Socket, Msg),

      Player = player:newPlayer(Simulation, PlayerName),
      receiveInput(Simulation, Player, Socket)
  end.

queryName(Socket) ->
  gen_tcp:send(Socket, "Please enter your name: "),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
        {error, quit};
    {tcp, Socket, Binary} ->
        % transform binary to string
        Pn = binary_to_list(Binary),
        % drop last character, which is ENTER
        PlayerName = string:substr(Pn, 1, string:length(Pn) - 1),
        {ok, PlayerName}
  after 
    10000 ->
      {error, timeout}
  end.

receiveInput(Simulation, Player, Socket) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
        gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
        gen_tcp:send(Socket, Msg),
        io:fwrite("Message: ~s ~n", [Msg]),
        receiveInput(Simulation, Player, Socket)
  end.
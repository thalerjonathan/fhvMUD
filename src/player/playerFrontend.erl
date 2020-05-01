-module(playerFrontend).

-export([new/2, sendText/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation, Socket) -> 
  % spawn a new client-process
  Pid = spawn(fun() -> init(Simulation, Socket) end),
  % give ownership of the socket to the client process
  Ret = gen_tcp:controlling_process(Socket, Pid),
  io:fwrite("controlling_process: ~w ~n", [Ret]),
  Pid.

sendText(Frontend, Text, FormatArgs) ->
  Frontend ! {sendText, io_lib:format(Text, FormatArgs)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Simulation, Socket) ->
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
      Player = player:new(Simulation, self(), PlayerName),
      process(Simulation, Player, Socket)

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

process(Simulation, Player, Socket) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);

    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      io:fwrite("Player typed: ~s ~n", [Msg]),
      process(Simulation, Player, Socket);
    
    {sendText, Text} ->
      gen_tcp:send(Socket, Text),
      process(Simulation, Player, Socket);

    {tcp_closed, Socket} ->
      io:fwrite("playerFrontend: socket ~w closed by player, removing player from simulation... ~n", [Socket]),
      player:remove(Player),
      simulation:removePlayer(Simulation, Player);

    Msg ->
      io:fwrite("playerFrontend received message: ~w ~n", [Msg]),
      process(Simulation, Player, Socket)
  end.
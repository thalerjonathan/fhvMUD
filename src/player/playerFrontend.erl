-module(playerFrontend).

-export([new/2, sendText/3]).

-record(ctx, {sock, sim, player, playername}).

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
      Msg = io_lib:format("Hello ~s! 
        Welcome to Chimera Multi-User Simulation!\n\n", 
        [PlayerName]),

      gen_tcp:send(Socket, Msg),

      Player = player:new(Simulation, self(), PlayerName),
      Context = #ctx{sim = Simulation, sock = Socket, player = Player, 
                     playername = PlayerName},
      process(Context)
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

process(Context) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);

    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handlePlayerInput(Msg, Context),
      process(Context);
    
    {sendText, Text} ->
      gen_tcp:send(Context#ctx.sock, Text),
      process(Context);

    {tcp_closed, Socket} ->
      io:fwrite("playerFrontend: socket ~w closed by player, removing player from simulation... ~n", [Socket]),
      player:remove(Context#ctx.player),
      simulation:removePlayer(Context#ctx.sim, Context#ctx.player);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
          "Error: PlayerFrontend ~w got unknown msg ~w~n.", 
          [self(), Other]),
      process(Context)
  end.

handlePlayerInput(Msg, Context) ->
  MsgTrim = string:trim(Msg),
  io:fwrite("Player typed: ~s ~n", [Msg]),

  Val = string:equal("say", MsgTrim, true),

  if 
    Val ->
      io:fwrite("Player typed: ~s ~n", [Msg]);
    true ->
      io:fwrite("Player typed: ~s ~n", [Msg])
  end,

  ok.
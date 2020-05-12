-module(player).

-export([new/2, remove/1, enteredRoom/3]).

-record(ctx, {sim, sock, name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOTES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% For now the socket communication and player logic is in one player module
% and process because it makes things easier without an additional process
% and module indirection. If in the future a separation makes sense, for example
% to support another protocol, then a refactoring can be done. But for now
% this technical debt is accepted for the sake of qicker prototyping and to 
% develop experience of developing a MUD

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation, Socket) ->
  Pid = spawn(fun() -> init(Simulation, Socket) end),
  % give ownership of the socket to the client process
  Ret = gen_tcp:controlling_process(Socket, Pid),
  io:fwrite("controlling_process: ~w ~n", [Ret]),
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
        Welcome to FHV Multi-User Dungeon!\n\n", 
        [PlayerName]),

      gen_tcp:send(Socket, Msg),

      Ctx = #ctx{sim = Simulation, sock = Socket, name = PlayerName},
      process(Ctx)
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

process(Ctx) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);

    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handlePlayerInput(Msg, Ctx),
      process(Ctx);
  
    {tcp_closed, Socket} ->
      io:fwrite("player: socket ~w closed by player, removing player from simulation... ~n", [Socket]),
      simulation:removePlayer(Ctx#ctx.sim, self());

    {enteredRoom, Room, RoomName} ->
      sendText(Ctx, "Entered Room ~w ~s ~n", [Room, RoomName]),
      process(Ctx);

    {remove} ->
      io:fwrite("player: removed ~w ~n", [Ctx#ctx.name]);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
          "Error: Player ~w got unknown msg ~w~n.", 
          [self(), Other]),
      process(Ctx)
  end.

handlePlayerInput(Msg, _Ctx) ->
  MsgTrim = string:trim(Msg),
  io:fwrite("Player typed: ~s ~n", [Msg]),

  Val = string:equal("say", MsgTrim, true),

  if 
    Val ->
      io:fwrite("Player typed say: ~s ~n", [Msg]);
    true ->
      io:fwrite("Player typed: ~s ~n", [Msg])
  end,

  ok.

sendText(Ctx, Text, FormatArgs) ->
  FormatedText = io_lib:format(Text, FormatArgs),
  gen_tcp:send(Ctx#ctx.sock, FormatedText).
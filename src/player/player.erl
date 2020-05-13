-module(player).

-export([new/2, remove/1, enteredRoom/3, sendText/3]).

-record(ctx, {sim, sock, room, name}).

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
  Pid.

remove(Player) ->
  Player ! {remove}.

sendText(Player, Text, FormatArgs) ->
  Player ! {sendText, Text, FormatArgs}.

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
      Lobby = enterLobby(Simulation, PlayerName),
      Ctx = #ctx{sim = Simulation, sock = Socket, room = Lobby, name = PlayerName},
      sendToSocket(Ctx, "Hello ~s! Welcome to FHV Multi-User Dungeon!\n\n", [PlayerName]),
      process(Ctx)
  end.

enterLobby(Simulation, PlayerName) ->
  Lobby = simulation:newPlayer(Simulation, self(), PlayerName),
  player:enteredRoom(self(), Lobby, "Lobby"),
  Lobby.

queryName(Socket) ->
  gen_tcp:send(Socket, "Please enter your name: "),

  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      {error, quit};

    {tcp, Socket, Binary} ->
      PlayerName = playerMessageToString(Binary),
      {ok, PlayerName}

  after 
    10000 ->
      {error, timeout}
  end.

process(Ctx) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      lobby:playerLeave(Ctx#ctx.room, self()),
      gen_tcp:close(Socket);

    {tcp, _Socket, Msg} ->
      handlePlayerMessage(Msg, Ctx),
      process(Ctx);
  
    {tcp_closed, Socket} ->
      io:fwrite("player: socket ~w closed by player, removing player from simulation... ~n", [Socket]),
      lobby:playerLeave(Ctx#ctx.room, self()),
      simulation:removePlayer(Ctx#ctx.sim, self());

    {enteredRoom, Room, RoomName} ->
      sendToSocket(Ctx, "Entered Room ~w ~s ~n", [Room, RoomName]),
      process(Ctx);

    {remove} ->
      lobby:playerLeave(Ctx#ctx.room, self()),
      io:fwrite("player: removed ~w ~n", [Ctx#ctx.name]);

    {sendText, Text, FormatArgs} ->
      sendToSocket(Ctx, Text, FormatArgs),
      process(Ctx);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
          "Error: Player ~w got unknown msg ~w~n.", 
          [self(), Other]),
      process(Ctx)
  end.

handlePlayerMessage(Binary, Ctx) ->
  Msg = playerMessageToString(Binary),
  io:fwrite("Player message: \"~s\" ~n", [Msg]),

  Tokens = string:split(Msg, " ", all),
  Cmd = hd(Tokens),

  case Cmd of
    "say" -> 
      io:fwrite("Player wants to say: ~s ~n", [Msg]),
      lobby:playerSays(Ctx#ctx.room, self(), tl(Tokens));
    _ ->
      io:fwrite("Player does not want to talk")
  end.

%startsWith(Str, Prefix) ->
%  string:equal(Prefix, string:slice(Str, 0, string:length(Prefix) - 1)).

playerMessageToString(Binary) ->
  % transform binary to string
  Pn = binary_to_list(Binary),
  % drop last character, which is ENTER
  Str = string:substr(Pn, 1, string:length(Pn) - 1),
  % trim: remove white spaces
  string:trim(Str).

sendToSocket(Ctx, Text, FormatArgs) ->
  FormatedText = io_lib:format(Text, FormatArgs),
  gen_tcp:send(Ctx#ctx.sock, FormatedText).
-module(player).

-export([new/3, remove/1, enteredRoom/3]).

-record(ctx, {sim, front, name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation, Frontend, PlayerName) ->
  Ctx = #ctx{sim=Simulation, front=Frontend, name=PlayerName},
  Pid = spawn(fun() -> process(Ctx) end),
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

process(Ctx) ->
  receive 
    {remove} ->
      io:fwrite("player: removed ~w ~n", [Ctx#ctx.name]);

    {enteredRoom, Room, RoomName} ->
      playerFrontend:sendText(Ctx#ctx.front, 
        "Entered Room ~w ~s ~n", 
        [Room, RoomName]);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error: Player ~w got unknown msg ~w~n.", 
        [self(), Other]),
      process(Ctx)
  end.
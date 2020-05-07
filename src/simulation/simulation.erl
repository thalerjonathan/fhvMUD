-module(simulation).

-export([new/0, newPlayer/2, removePlayer/2]).

-record(ctx, {lobby}).

% TODO follow http://www.erlang.se/doc/programming_rules.shtml

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
  Pid = spawn(fun() -> init() end),
  io:fwrite("Simulation: ~p ~n", [Pid]),
  Pid.

newPlayer(Simulation, Player) ->
  Simulation ! { newPlayer, Player }.

removePlayer(Simulation, Player) ->
  Simulation ! { removePlayer, Player }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  Lobby = loadWorld(),
  io:fwrite("Simulation running ~n"),
  process(#ctx{lobby=Lobby}).

loadWorld() ->
  Lobby = lobby:new(self()),
  Lobby.

process(Ctx) ->
  receive 
    {newPlayer, Player} ->
      io:fwrite("New Player ~w ~n", [Player]),
      player:enteredRoom(Player, Ctx#ctx.lobby, "Lobby"),
      lobby:playerEnter(Ctx#ctx.lobby, Player),
      process(Ctx);

    {removePlayer, Player} ->
      io:fwrite("simulation: removed player ~w... ~n", [Player]),
      process(Ctx);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error: Simulation ~w got unknown msg ~w~n.", 
        [self(), Other]),
      process(Ctx)
  end.
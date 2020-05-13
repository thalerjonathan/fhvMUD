-module(simulation).

-export([new/0, newPlayer/3, removePlayer/2]).

-record(ctx, {lobby}).

% TODO follow http://www.erlang.se/doc/programming_rules.shtml

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
  Pid = spawn(fun() -> init() end),
  io:fwrite("Simulation: ~p ~n", [Pid]),
  Pid.

newPlayer(Simulation, Player, PlayerName) ->
  Simulation ! { newPlayer, Player, PlayerName },
  receive
    {lobby, Lobby} ->
      Lobby;

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error in newPlayer: expected Lobby message~n.", 
        [self(), Other]),
      error
  end.

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
    {newPlayer, Player, PlayerName} ->
      io:fwrite("New Player ~w with name ~s ~n", [Player, PlayerName]),
      lobby:playerEnter(Ctx#ctx.lobby, Player, PlayerName),
      Player ! {lobby, Ctx#ctx.lobby},
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
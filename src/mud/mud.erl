-module(mud).

-export([new/0, newPlayer/3, removePlayer/2]).

-record(ctx, {lobby}).

% TODO follow http://www.erlang.se/doc/programming_rules.shtml

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new() ->
  Pid = spawn(fun() -> init() end),
  io:fwrite("fhvMUD: ~p ~n", [Pid]),
  Pid.

newPlayer(Mud, Player, PlayerName) ->
  Mud ! { newPlayer, Player, PlayerName },
  receive
    {lobby, Lobby} ->
      Lobby;

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error in newPlayer: expected Lobby message~n.", 
        [self(), Other]),
      error
  end.

removePlayer(Mud, Player) ->
  Mud ! { removePlayer, Player }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
  Lobby = loadWorld(),
  io:fwrite("fhvMUD running ~n"),
  process(#ctx{lobby=Lobby}).

loadWorld() ->
  Lobby = room:new(self()),
  Lobby.

process(Ctx) ->
  receive 
    {newPlayer, Player, PlayerName} ->
      io:fwrite("New Player ~w with name ~s ~n", [Player, PlayerName]),
      room:playerEnter(Ctx#ctx.lobby, Player, PlayerName),
      Player ! {lobby, Ctx#ctx.lobby},
      process(Ctx);

    {removePlayer, Player} ->
      io:fwrite("fhvMUD: removed player ~w... ~n", [Player]),
      process(Ctx);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error: fhvMUD ~w got unknown msg ~w~n.", 
        [self(), Other]),
      process(Ctx)
  end.
-module(lobby).

-export([new/1, playerEnter/3, playerSays/3, playerLeave/2]).

-record(ctx, {sim, actors}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation) ->
  Actors = maps:new(),
  Pid = spawn(fun() -> process(#ctx{sim=Simulation, actors=Actors}) end),
  Pid.

playerEnter(Lobby, Player, PlayerName) ->
  Lobby ! {playerEnter, Player, PlayerName}.

playerSays(Lobby, Player, Text) ->
  Lobby ! {playerSays, Player, Text}.

playerLeave(Lobby, Player) ->
  Lobby ! {playerLeave, Player}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(Ctx) ->
  receive
    {playerEnter, Player, PlayerName} ->
      Actors = Ctx#ctx.actors,
      ActorsAdded = Actors#{ Player => PlayerName },
      
      %io:fwrite("Lobby: player ~s entered ~n", [PlayerName]),
      sendToAllActors(Ctx, "Lobby: ~s enterd the room ~n", [PlayerName]),

      process(Ctx#ctx{actors=ActorsAdded});

    {playerLeave, Player} ->
      Lookup = maps:find(Player, Ctx#ctx.actors),
      case Lookup of
        {ok, PlayerName} ->
          %io:fwrite("Lobby: player ~s says: ~s ~n", [PlayerName, Text]),
          sendToAllActors(Ctx, "Lobby: ~s left ~n", [PlayerName]);

        error ->
          ok
      end;

    {playerSays, Player, Text} ->
      Lookup = maps:find(Player, Ctx#ctx.actors),
      case Lookup of
        {ok, PlayerName} ->
          %io:fwrite("Lobby: player ~s says: ~s ~n", [PlayerName, Text]),
          sendToAllActors(Ctx, "Lobby: ~s says \"~s\" ~n", [PlayerName, Text]);

        error ->
          ok
      end,
  
      process(Ctx);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error: Lobby ~w got unknown msg ~w~n.", 
        [self(), Other]),
      process(Ctx)
  end.

sendToAllActors(Ctx, Text, FormatArgs) ->
  ActorPids = maps:keys(Ctx#ctx.actors),
  lists:map(fun(Pid) -> player:sendText(Pid, Text, FormatArgs) end, ActorPids).
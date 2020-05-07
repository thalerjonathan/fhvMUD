-module(lobby).

-export([new/1, playerEnter/2]).

-record(ctx, {sim}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Simulation) ->
  Pid = spawn(fun() -> process(#ctx{sim=Simulation}) end),
  Pid.

playerEnter(Lobby, Player) ->
  Lobby ! {playerEnter, Player}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(Ctx) ->
  receive
    {playerEnter, Player} ->
      io:fwrite("Lobby: player ~w entered ~n", [Player]),
      process(Ctx);

    Other -> % Flushes the message queue.
      error_logger:error_msg(
        "Error: Lobby ~w got unknown msg ~w~n.", 
        [self(), Other]),
      process(Ctx)
  end.

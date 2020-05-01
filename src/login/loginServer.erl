-module(loginServer).

-export([start/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Simulation) -> 
  % needs active true, otherwise cannot transfer ownership of socket
  % to player frontend
  {ok, ListenSocket} = gen_tcp:listen(8080, [{active,true}, 
      {reuseaddr, true}, {packet, line}, binary]),
  spawn(fun() -> process(Simulation, ListenSocket) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(Simulation, ListenSocket) ->
  Ret = gen_tcp:accept(ListenSocket),
  case Ret of 
    {ok, Socket} ->
      playerFrontend:new(Simulation, Socket),
      process(Simulation, ListenSocket);

    {error,closed} ->
      io:fwrite("closed login server socket~n")
      
  end.
  
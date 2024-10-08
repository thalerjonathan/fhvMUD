-module(loginServer).

-export([start/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Mud) -> 
  % needs active true, otherwise cannot transfer ownership of socket
  % to player frontend
  {ok, ListenSocket} = gen_tcp:listen(8080, [{active,true}, 
      {reuseaddr, true}, {packet, line}, binary]),
  spawn(fun() -> process(Mud, ListenSocket) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process(Mud, ListenSocket) ->
  Ret = gen_tcp:accept(ListenSocket),
  case Ret of 
    {ok, Socket} ->
      player:new(Mud, Socket),
      process(Mud, ListenSocket);

    {error,closed} ->
      io:fwrite("closed login server socket~n")
      
  end.
  
-module(loginServer).

-export([startServer/1, serverProc/2]).

startServer(Simulation) -> 
    % needs active true, otherwise cannot transfer ownership of socket
    % to player frontend
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active,true}, 
        {reuseaddr, true}, {packet, line}, binary]),
    spawn(?MODULE, serverProc, [Simulation, ListenSocket]).

serverProc(Simulation, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    playerFrontend:spawnPlayer(Simulation, Socket),
    serverProc(Simulation, ListenSocket).
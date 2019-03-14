-module(loginServer).

-export([startServer/1, serverProc/2]).

startServer(Simulation) -> 
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active,false}, 
        {reuseaddr, true}, {packet, line}, binary]),
    spawn(?MODULE, serverProc, [Simulation, ListenSocket]).

serverProc(Simulation, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % send welcome message
    gen_tcp:send(Socket, "Welcome to Chimera Multi-User Simulation!\n"),
    c:flush(),
    playerFrontend:spawnPlayer(Simulation, Socket),
    serverProc(Simulation, ListenSocket).
-module(loginServer).

-export([startServer/1, serverProc/2]).

%%====================================================================
%% Internal functions
%%====================================================================
startServer(Sid) -> 
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active,false}, 
        {reuseaddr, true}, {packet, line}, binary]),
    spawn(?MODULE, serverProc, [Sid, ListenSocket]).

serverProc(Sid, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % send welcome message
    gen_tcp:send(Socket, "Welcome to Chimera Multi-User Simulation!\n"),
    c:flush(),
    playerfrontend:newPlayer(Sid, Socket),
    serverProc(Sid, ListenSocket).
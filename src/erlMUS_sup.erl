%%%-------------------------------------------------------------------
%% @doc erlMUS top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlMUS_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, server/1, clientProc/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    startServer(),
    {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
startServer() -> 
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active,false}, 
        {reuseaddr, true}, {packet, line}, binary]),
    spawn_link(?MODULE, server, [ListenSocket]).

server(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    % send welcome message
    gen_tcp:send(Socket, "Welcome to Erlang Multi-User Simulation!\n"),
    c:flush(),
    % spawn a new client-process
    Pid = spawn_link(?MODULE, clientProc, [Socket]),
    % give ownership of the socket to the client process
    gen_tcp:controlling_process(Socket, Pid),
    server(ListenSocket).

clientProc(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            io:fwrite("Message: ~s ~n", [Msg]),
            clientProc(Socket)
    end.

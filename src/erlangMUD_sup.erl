%%%-------------------------------------------------------------------
%% @doc erlangMUD top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlangMUD_sup).

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
    {ok, ListenSocket} = gen_tcp:listen(8080, [{active,true}, {reuseaddr,
true}, binary]),
    spawn(erlangMUD, server, ListenSocket).

server(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(erlangMUD, clientProc, Socket),
    server(ListenSocket).

clientProc(Socket) -> 
    gen_tcp:send(Socket, "Welcome to erlangMUD!"),
    c:flush(),
    gen_tcp:close(Socket).

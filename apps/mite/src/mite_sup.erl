%%%-------------------------------------------------------------------
%% @doc mite top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mite_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Clients} = application:get_env(mite, clients),
    Childs = lists:map(fun to_client/1, Clients),
    {ok, {{one_for_one, 5, 30}, Childs}}.

%%====================================================================
%% Internal functions
%%====================================================================

to_client(#{local_port := InPort}=Client) ->
    #{id => InPort,
      start => {mite_client, start_link, [Client]}}.

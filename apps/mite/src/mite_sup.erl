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
    {ok, Connections} =
        case mite_config:from_env() of
            {ok, _} = C ->
                C;
            ok ->
                application:get_env(mite, clients);
            {error, _} = E ->
                E
        end,
    Childs = lists:map(fun to_client/1, Connections),
    {ok, {{one_for_one, 5, 30}, Childs}}.

%%====================================================================
%% Internal functions
%%====================================================================

to_client(#{local_port := InPort} = Connection) ->
    #{
        id => InPort,
        start => {mite_acceptor, start_link, [Connection]}
    }.

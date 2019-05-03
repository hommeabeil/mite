-module(mite_proxy).

-behavior(gen_server).

-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3]).

-export([start_link/1,
         connect/2]).

-record(state, {config,
                in_socket,
                out_socket}).

%%====================================================================
%% Public API
%%====================================================================

-spec start_link(#{}) -> {ok, pid()} | {error, term()} | ignore.
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

-spec connect(pid(), term()) -> ok.
connect(Pid, Socket) ->
    gen_server:cast(Pid, {connect, Socket}).


%%====================================================================
%% gen_server handle
%%====================================================================

init([Config]) ->
    {ok, #state{config = Config}}.

handle_info({ssl, OutS, Data}, #state{out_socket = OutS}=State) ->
    ok = ssl:send(State#state.in_socket, Data),
    {noreply, State};
handle_info({ssl, InS, Data}, #state{in_socket=InS}=State) ->
    ok = ssl:send(State#state.out_socket, Data),
    {noreply, State};
handle_info({ssl_closed, _}, State) ->
    dirty_close(State#state.out_socket),
    dirty_close(State#state.in_socket),
    %% timer:sleep(5000),
    {stop, normal, State#state{out_socket = undefied,
                               in_socket = undefied}};
handle_info(Info, State) ->
    io:format(user, "[Module:~p Line:~p] ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.


dirty_close(undefied) ->
    ok;
dirty_close(S) ->
    ssl:close(S).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({connect, InSocket}, #state{config = Config}=State) ->
    #{out_port := P, out_host := H} = Config,
    {ok, S} = ssl:connect(H, P, [{active, true}]),
    ssl:setopts(InSocket, [{active, true}]),
    {noreply, State#state{in_socket = InSocket, out_socket = S}};
handle_cast(_Request, State) ->
    {noreply, State}.

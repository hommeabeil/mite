-module(mite_proxy).
-include_lib("kernel/include/logger.hrl").

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
handle_info({ssl_closed, S}, #state{in_socket=In, out_socket=Out, config=Config}=State) ->
    #{remote_port := OutPort, local_port := InPort, remote_host := Host} = Config,
    case S of
        In ->
            ?LOG_NOTICE(#{what => "Connection close", closed_by => client, config => Config});
        Out ->
            ?LOG_NOTICE(#{what => "Connection close", closed_by => remote, config => Config})
    end,

    dirty_close(State#state.out_socket),
    dirty_close(State#state.in_socket),
    {stop, normal, State#state{out_socket = undefied,
                               in_socket = undefied}};
handle_info(Info, State) ->
    ?LOG_NOTICE(#{what => message, message => Info}),
    {noreply, State}.


dirty_close(undefied) ->
    ok;
dirty_close(S) ->
    ssl:close(S).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({connect, InSocket}, #state{config = Config}=State) ->
    #{remote_port := P, remote_host := H} = Config,
    ?LOG_DEBUG(#{what => try_connect, host => H, port => P}),
    SSLOptions = generate_options(Config),
    {ok, S} = ssl:connect(H, P, [{active, true}] ++ SSLOptions),
    ?LOG_NOTICE(#{what => connected, config => Config}),
    ssl:setopts(InSocket, [{active, true}]),
    {noreply, State#state{in_socket = InSocket, out_socket = S}};
handle_cast(_Request, State) ->
    {noreply, State}.

generate_options(#{sni := SNI}) ->
    [{server_name_indication, SNI}];
generate_options(_) ->
    [].


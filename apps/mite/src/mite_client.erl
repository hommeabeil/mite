-module(mite_client).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_link/0]).

-record(state, {listen_socket,
                in_socket,
                out_socket}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    self() ! listen,
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Call, State) ->
    {noreply, State}.

handle_info(listen, State) ->
    Priv = code:priv_dir(mite),
    CertFile = filename:join([Priv, "cert.pem"]),
    KeyFile = filename:join([Priv, "key.pem"]),
    {ok, P} = application:get_env(in_port),
    {ok, _} = file:read_file(KeyFile),
    {ok, S} = ssl:listen(P, [{certfile, CertFile}, {keyfile, KeyFile}, {active, false}, {ciphers, [{rsa,
                                                                                                    aes_128_cbc,
                                                                                                    sha,
                                                                                                    default_prf}]}
                               ]),
    self() ! accept,
    {noreply, State#state{listen_socket = S}};
handle_info(accept, State) ->
    {ok, SS} = ssl:transport_accept(State#state.listen_socket),
    ok = ssl:ssl_accept(SS),
    self() ! start_connection,
    ssl:setopts(SS, [{active, true}]),
    io:format("~p~n", ["Connected"]),
    {noreply, State#state{in_socket = SS}};
handle_info(start_connection, State) ->
    {ok, P} = application:get_env(out_port),
    {ok, H} = application:get_env(out_host),
    {ok, S} = ssl:connect(H, P, [{active, true}]),
    {noreply, State#state{out_socket = S}};

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
    self() ! accept,
    {noreply, State#state{out_socket = undefied,
                          in_socket = undefied}};
handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.


dirty_close(undefied) ->
    ok;
dirty_close(S) ->
    ssl:close(S).

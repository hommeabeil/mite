-module(mite_client).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_link/1]).

-record(state, {config,
                listen_socket,
                in_socket,
                out_socket}).

start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

init([Config]) ->
    process_flag(trap_exit, true),
    self() ! listen,
    {ok, #state{config = Config}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Call, State) ->
    {noreply, State}.

handle_info(listen, #state{config = #{in_port := P}}=State) ->
    Priv = code:priv_dir(mite),
    CertFile = filename:join([Priv, "cert.pem"]),
    KeyFile = filename:join([Priv, "key.pem"]),
    {ok, _} = file:read_file(KeyFile),
    {ok, S} = ssl:listen(P, [{certfile, CertFile},
                             {keyfile, KeyFile},
                             {active, false},
                             {ciphers, [{rsa,
                                         aes_128_cbc,
                                         sha,
                                         default_prf}]}
                            ]),
    self() ! accept,
    {noreply, State#state{listen_socket = S}};
handle_info(accept, State) ->
    {ok, SS} = ssl:transport_accept(State#state.listen_socket),
    ok = ssl:ssl_accept(SS),
    {ok, Pid} = mite_proxy:start_link(State#state.config),
    unlink(Pid),
    ok = ssl:controlling_process(SS, Pid),
    ok = mite_proxy:connect(Pid, SS),
    self() ! accept,
    io:format(user, "[Module:~p Line:~p] ~p~n", [?MODULE, ?LINE, "Connected"]),
    {noreply, State};

handle_info(Info, State) ->
    io:format(user, "[Module:~p Line:~p] ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

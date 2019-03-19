-module(mite_client).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_link/0]).

-record(state, {in_socket,
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
    recon_trace:calls({ssl_certificate, '_', '_'}, 10),
    Priv = code:priv_dir(mite),
    CertFile = filename:join([Priv, "cert.pem"]),
    KeyFile = filename:join([Priv, "key.pem"]),
    {ok, _} = file:read_file(KeyFile),
    io:format("cert: ~p~n", [CertFile]),
    {ok, S} = ssl:listen(8081, [{certfile, CertFile}, {keyfile, KeyFile}, {active, false}, {ciphers, [{rsa,
                                                                                                       aes_128_cbc,
                                                                                                        sha,
                                                                                                        default_prf}]}
                               ]),
    {ok, SS} = ssl:transport_accept(S),
    ok = ssl:ssl_accept(SS),
    self() ! start_connection,
    ssl:setopts(SS, [{active, true}]),
    %% {ok, S} = gen_tcp:listen(8081, [{active, true}]),
    %% {ok, SS} = gen_tcp:accept(S),
    io:format("~p~n", ["Connected"]),
    {noreply, State#state{in_socket = SS}};
handle_info(start_connection, State) ->
    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, 8080, [{active, true}]),
    {noreply, State#state{out_socket = S}};

handle_info({tcp, InS, Data}, #state{in_socket = InS}=State) ->
    %% ok = gen_tcp:send(#state.out_socket, Data),
    {noreply, State};
handle_info({ssl, _, Data}, State) ->
    ok = gen_tcp:send(State#state.out_socket, Data),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

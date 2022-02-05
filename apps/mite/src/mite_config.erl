-module(mite_config).

-type config() :: #{connections := [connection()]}.

-type connection() :: #{
    local_port := inet:port_number(),
    remote_port := inet:port_number(),
    remote_host := inet:ip_address() | inet:hostname()
}.

-type err() :: {error, term()}.

-export_type([config/0]).

-export([
    from_env/0,
    connections_from_string/1
]).

%% format is local:port:remote:port
%% for now we do not support ipv6, but it will come in the from [::]
-spec from_env() -> ok | {ok, config()} | {error, term()}.
from_env() ->
    case os:getenv("MITE_CONNECTIONS") of
        false ->
            ok;
        Value ->
            connections_from_string(Value)
    end.

-spec connections_from_string(string() | binary()) -> connection().
connections_from_string("") ->
    {ok, []};
connections_from_string(B) when is_binary(B) ->
    connections_from_string(binary_to_list(B));
connections_from_string(Connections) when is_list(Connections) ->
    ConnectionsString = string:lexemes(Connections, ","),
    lists:foldr(fun connection_from_string/2, {ok, []}, ConnectionsString).

-spec connection_from_string(string(), {ok, [connection()]} | err()) -> {ok, [connection()]} | err().
connection_from_string(_, {error, _} = E) ->
    E;
connection_from_string(ConnectionString, {ok, List}) ->
    case connection_from_string(ConnectionString) of
        {ok, Connection} ->
            {ok, [Connection | List]};
        {error, _} = E ->
            E
    end.

-spec connection_from_string(string()) -> {ok, connection()} | err().
connection_from_string(ConnectionString) ->
    Parts = string:lexemes(ConnectionString, ":"),
    case to_connection(Parts) of
        {ok, Connection} ->
            {ok, Connection};
        {error, _} = E ->
            E
    end.

to_connection([LocalHost, LocalPort, RemoteHost, RemotePort]) ->
    Map = #{
        local_host => LocalHost,
        local_port => LocalPort,
        remote_port => RemotePort,
        remote_host => RemoteHost
    },
    do_convert(Map);
to_connection([LocalPort, RemoteHost, RemotePort]) ->
    Map = #{
        local_port => LocalPort,
        remote_port => RemotePort,
        remote_host => RemoteHost
    },
    do_convert(Map);
to_connection(_) ->
    {error, invalid_connection}.

do_convert(Map) ->
    maps:fold(fun do_convert/3, {ok, default()}, Map).

do_convert(_, _, {error, _} = E) ->
    E;
do_convert(local_host, V, {ok, M}) when is_tuple(V) ->
    {ok, M};
do_convert(local_host = K, V, {ok, M}) ->
    case inet:parse_strict_address(V) of
        {ok, Ip} ->
            {ok, M#{K => Ip}};
        {error, einval} ->
            {error, {invalid_local_host, V}}
    end;
do_convert(local_port, V, {ok, _} = Acc) when is_integer(V) ->
    Acc;
do_convert(local_port = K, V, {ok, M}) ->
    case do_parse_port(V) of
        {ok, Port} ->
            {ok, M#{K => Port}};
        {error, _} = E ->
            E
    end;
do_convert(remote_port = K, V, {ok, M}) ->
    case do_parse_port(V) of
        {ok, Port} ->
            {ok, M#{K => Port}};
        {error, _} = E ->
            E
    end;
do_convert(remote_host = K, V, {ok, M}) ->
    case inet:parse_strict_address(V) of
        {ok, Ip} ->
            {ok, M#{K => Ip}};
        {error, einval} ->
            {ok, M#{K => V}}
    end.

do_parse_port(V) ->
    case string:to_integer(V) of
        {Port, ""} when Port >= 0, Port =< 65535 ->
            {ok, Port};
        {_Port, ""} ->
            {error, {port_is_out_of_range, V}};
        {error, _} ->
            {error, {invalid_port_number, V}};
        {_Port, _Rest} ->
            {error, {invalid_port_number, V}}
    end.

default() ->
    #{
        local_host => {0, 0, 0, 0},
        local_port => 0
    }.

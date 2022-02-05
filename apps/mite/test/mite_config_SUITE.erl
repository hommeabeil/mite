-module(mite_config_SUITE).
-compile(export_all).
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        from_empty_string,
        from_simple_string,
        can_omit_local_host,
        can_use_remote_hostname,
        canot_use_ipv6,
        can_use_binary,
        can_read_multiple_connections,
        return_error_on_bad_port
    ].

from_empty_string(_Config) ->
    ?assertEqual({ok, []}, mite_config:connections_from_string("")).

from_simple_string(_Config) ->
    ?assertEqual(
        {ok, [#{local_host => {127, 0, 0, 1}, local_port => 8080, remote_port => 443, remote_host => {10, 0, 0, 1}}]},
        mite_config:connections_from_string("127.0.0.1:8080:10.0.0.1:443")
    ).

can_omit_local_host(_Config) ->
    ?assertEqual(
        {ok, [#{local_host => {0, 0, 0, 0}, local_port => 8080, remote_port => 443, remote_host => {10, 0, 0, 1}}]},
        mite_config:connections_from_string("8080:10.0.0.1:443")
    ).

can_use_remote_hostname(_Config) ->
    ?assertEqual(
        {ok, [#{local_host => {0, 0, 0, 0}, local_port => 8080, remote_port => 443, remote_host => "www.yep.com"}]},
        mite_config:connections_from_string("8080:www.yep.com:443")
    ).

canot_use_ipv6(_Config) ->
    ?assertMatch(
        {error, _},
        mite_config:connections_from_string("8080::::443")
    ).

return_error_on_bad_port(_Config) ->
    ?assertMatch(
        {error, _},
        mite_config:connections_from_string("8080:www.yep.com:999999")
    ).

can_use_binary(_Config) ->
    ?assertEqual(
        {ok, [#{local_host => {0, 0, 0, 0}, local_port => 8080, remote_port => 443, remote_host => "www.yep.com"}]},
        mite_config:connections_from_string(<<"8080:www.yep.com:443">>)
    ).

can_read_multiple_connections(_Config) ->
    ?assertEqual(
        {ok, [
            #{local_host => {0, 0, 0, 0}, local_port => 8080, remote_port => 443, remote_host => "www.yep.com"},
            #{local_host => {0, 0, 0, 0}, local_port => 8081, remote_port => 443, remote_host => "www.yo.com"}
        ]},
        mite_config:connections_from_string("8080:www.yep.com:443,8081:www.yo.com:443")
    ).

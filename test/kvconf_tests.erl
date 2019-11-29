-module(kvconf_tests).

-include_lib("eunit/include/eunit.hrl").

smoke_test() ->
    ok = kvconf:open(kernel,
                     [{two_digits, {integer, 10, 99}, required},
                      {url, http_uri, optional, "boom"},
                      {string, string, optional, "bang"},
                      {bool_true, boolean, required},
                      {bool_false, boolean, optional},
                      {ipv4, ipv4_address, required},
                      {ipv6, ipv6_address, required},
                      {ipv4_port, ipv4_address_and_port_number, required}],
                     <<"test/smoke_test.conf">>),

    ?assertEqual(71, get_value(two_digits)),
    ?assertEqual(<<"http://www.example.com/">>, get_value(url)),
    ?assertEqual(true, get_value(bool_true)),
    ?assertEqual(false, get_value(bool_false)),
    ?assertEqual({192, 168, 0, 1}, get_value(ipv4)),
    ?assertEqual({0,0,0,0,0,0,0,1}, get_value(ipv6)),
    ?assertEqual(<<"foo bar baz">>, get_value(string)),
    ?assertEqual({{127, 0, 0, 1}, 777}, get_value(ipv4_port)),
    ok.


get_value(Key) ->
    ?debugVal(Key),
    {ok, Value} = application:get_env(kernel, Key),
    Value.


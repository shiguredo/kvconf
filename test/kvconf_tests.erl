-module(kvconf_tests).

-include_lib("eunit/include/eunit.hrl").


smoke_test() ->
    {ok, Binary} = file:read_file(<<"test/smoke_test.conf">>),
    ok = kvconf:initialize(
           [{two_digits,   {integer, 10, 99},            required                              },
            {url,          http_uri,                     optional, <<"http://foo.example.com">>},
            {string,       string,                       optional, <<"bang">>                  },
            {string2,      string,                       optional, <<"bang2">>                 },
            {string3,      string,                       optional                              },
            {empty_string, string,                       optional, <<"boom!!">>                },
            {bool_true,    boolean,                      required                              },
            {bool_false,   boolean,                      optional                              },
            {bool_default, boolean,                      optional, true                        },
            {ipv4,         ipv4_address,                 required                              },
            {ipv6,         ipv6_address,                 required                              },
            {ipv4_port,    ipv4_address_and_port_number, required                              }],
           Binary),

    ?assertEqual(71, get_value(two_digits)),
    ?assertEqual(<<"http://www.example.com/">>, get_value(url)),
    ?assertEqual(true, get_value(bool_true)),
    ?assertEqual(false, get_value(bool_false)),
    ?assertEqual(true, get_value(bool_default)),
    ?assertEqual({192, 168, 0, 1}, get_value(ipv4)),
    ?assertEqual({0,0,0,0,0,0,0,1}, get_value(ipv6)),
    ?assertEqual(<<"foo bar baz">>, get_value(string)),
    ?assertEqual(<<"bang2">>, get_value(string2)),
    ?assertEqual(undefined, get_value(string3)),
    ?assertEqual(<<>>, get_value(empty_string)),
    ?assertEqual({{127, 0, 0, 1}, 777}, get_value(ipv4_port)),
    ok.


get_value(Key) ->
    kvconf:get_value(Key).

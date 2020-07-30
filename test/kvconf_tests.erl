-module(kvconf_tests).

-include_lib("eunit/include/eunit.hrl").


smoke_test() ->
    {ok, Binary} = file:read_file(<<"test/smoke_test.conf">>),
    ok = kvconf:initialize(
           [{two_digits,   {integer, 10, 99},            required                              },
            {float_foo,    {float, -10, 10},             required                              },
            {float_bar,    {float, -10, 10},             required                              },
            {url,          http_uri,                     optional, <<"http://foo.example.com">>},
            {atom_default, {atom, [foo, bar]},           optional, foo                         },
            {atom_foo,     {atom, [foo, bar]},           optional, foo                         },
            {atom_bar,     {atom, [foo, bar]},           optional, foo                         },
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
    ?assertEqual(1.2, get_value(float_foo)),
    ?assertEqual(2.2017764, get_value(float_bar)),
    ?assertEqual(<<"http://www.example.com/">>, get_value(url)),
    ?assertEqual(true, get_value(bool_true)),
    ?assertEqual(false, get_value(bool_false)),
    ?assertEqual(true, get_value(bool_default)),
    ?assertEqual({192, 168, 0, 1}, get_value(ipv4)),
    ?assertEqual({0,0,0,0,0,0,0,1}, get_value(ipv6)),
    ?assertEqual(foo, get_value(atom_default)),
    ?assertEqual(foo, get_value(atom_foo)),
    ?assertEqual(bar, get_value(atom_bar)),
    ?assertEqual(<<"foo bar baz">>, get_value(string)),
    ?assertEqual(<<"bang2">>, get_value(string2)),
    ?assertEqual(not_found, get_value(string3)),
    ?assertEqual(<<>>, get_value(empty_string)),
    ?assertEqual({{127, 0, 0, 1}, 777}, get_value(ipv4_port)),
    ok.


invalid_value_test() ->
    Line = <<"a = Vuls">>,
    {error, {invalid_value, Line, 2}} = kvconf:initialize(
                                          [{a, boolean, optional}],
                                          <<"\n", Line/binary, "\n">>),
    ok.


get_value(Key) ->
    kvconf:get_value(Key).

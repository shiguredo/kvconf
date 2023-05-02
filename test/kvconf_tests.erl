-module(kvconf_tests).

-include("kvconf.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(kvconf, [initialize/2, get_value/1, unset_value/1]).


smoke_test() ->
    {ok, Binary} = file:read_file(<<"test/smoke_test.conf">>),
    {ok, [], []} =
        initialize(
          [#kvc{
             key = two_digits,
             type = #kvc_integer{min = 10, max = 99},
             required = true
            },

           #kvc{
             key = float_foo,
             type = #kvc_float{min = -10, max = 10},
             required = true
            },
           #kvc{
             key = float_bar,
             type = #kvc_float{min = -10, max = 10},
             required = true
            },

           #kvc{
             key = url,
             type = #kvc_http_uri{},
             default = <<"http://foo.example.com">>
            },

           #kvc{
             key = atom_default,
             type = #kvc_atom{candidates = [foo, bar]},
             default = foo
            },
           #kvc{
             key = atom_foo,
             type = #kvc_atom{candidates = [foo, bar]},
             default = foo
            },
           #kvc{
             key = atom_bar,
             type = #kvc_atom{candidates = [foo, bar]},
             default = foo
            },
           %% required = false を明示的にしてみる
           #kvc{
             key = atom_by_bin,
             type = #kvc_atom{
                      candidates = [{<<"Portrait">>, portrait},
                                    {<<"Landscape">>, landscape}]
                     },
             required = false,
             default = landscape
            },
           %% required = false を明示的にしてみる
           #kvc{
             key = atom_by_bin2,
             type = #kvc_atom{candidates = [{<<"A4">>, a4}, {<<"B4">>, b4}]},
             required = false,
             default = a4
            },

           #kvc{
             key = string,
             type = #kvc_string{},
             default = <<"bang">>
            },
           #kvc{
             key = string2,
             type = #kvc_string{},
             default = <<"bang2">>
            },
           #kvc{key = string3, type = #kvc_string{}},
           #kvc{
             key = empty_string,
             type = #kvc_string{},
             default = <<"boom!!">>
            },

           #kvc{
             key = bool_true,
             type = #kvc_boolean{},
             required = true
            },
           #kvc{key = bool_false, type = #kvc_boolean{}},
           #kvc{
             key = bool_default,
             type = #kvc_boolean{},
             default = true
            },

           #kvc{
             key = interval_ms,
             type = #kvc_interval{
                      min = {10, ms},
                      max = {100, ms},
                      out_time_unit = millisecond
                     },
             default = {50, ms}
            },

           #kvc{
             key = port,
             type = #kvc_port_number{},
             default = 3000
            },

           #kvc{
             key = ipv4,
             type = #kvc_ipv4_address{},
             required = true
            },

           #kvc{
             key = ipv6,
             type = #kvc_ipv6_address{},
             required = true
            },

           #kvc{
             key = list_ipv4,
             type = #kvc_list_ipv4_address{},
             default = []
            },

           #kvc{
             key = list_ipv6,
             type = #kvc_list_ipv6_address{},
             default = []
            }],
          Binary),

    ?assertEqual(71, get_value(two_digits)),
    ?assertEqual(1.2, get_value(float_foo)),
    ?assertEqual(2.2017764, get_value(float_bar)),
    ?assertEqual(<<"http://www.example.com/">>, get_value(url)),
    ?assertEqual(true, get_value(bool_true)),
    ?assertEqual(false, get_value(bool_false)),
    ?assertEqual(true, get_value(bool_default)),
    ?assertEqual({192, 168, 0, 1}, get_value(ipv4)),
    ?assertEqual({0, 0, 0, 0, 0, 0, 0, 1}, get_value(ipv6)),
    ?assertEqual(foo, get_value(atom_default)),
    ?assertEqual(foo, get_value(atom_foo)),
    ?assertEqual(bar, get_value(atom_bar)),
    ?assertEqual(landscape, get_value(atom_by_bin)),
    ?assertEqual(a4, get_value(atom_by_bin2)),
    ?assertEqual(7777, get_value(port)),
    ?assertEqual(<<"foo bar baz">>, get_value(string)),
    ?assertEqual(<<"bang2">>, get_value(string2)),
    ?assertEqual(not_found, get_value(string3)),
    ?assertEqual(<<>>, get_value(empty_string)),

    ?assertEqual([{192, 0, 2, 1}, {192, 0, 2, 3}], get_value(list_ipv4)),
    ?assertEqual([{0, 0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 0, 0, 1}], get_value(list_ipv6)),

    %% unset_value のテストをこっそりと
    ?assertEqual(ok, unset_value(list_ipv4)),
    ?assertEqual(not_found, get_value(list_ipv4)),

    ok.


invalid_value_test() ->
    Line = <<"a = Vuls">>,
    {error, {invalid_value, Line, 2}} = initialize([#kvc{key = a, type = #kvc_boolean{}}], <<"\n", Line/binary, "\n">>),
    ok.


missing_key_test() ->
    Line = <<"a = Vuls">>,
    {error, {missing_required_key, b, 4}} = initialize([#kvc{
                                                          key = b,
                                                          required = true,
                                                          type = #kvc_boolean{}
                                                         }],
                                                       <<"\n", Line/binary, "\n">>),
    ok.

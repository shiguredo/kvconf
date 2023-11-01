-module(kvconf_validate).

-export([validate/3]).

-include("kvconf.hrl").


-spec validate(non_neg_integer(), COnfigurations :: map(), [#kvc{}]) ->
          ok |
          {error, {Reason :: atom(), Line :: any(), LineNumber :: non_neg_integer()}}.
validate(_LastLineNumber, _Configurations, []) ->
    ok;
validate(LastLineNumber, Configurations, [#kvc{key = Key} = Kvc | KvcList]) ->
    case maps:get(atom_to_binary(Key, utf8), Configurations, not_found) of
        not_found ->
            %% default を引っ張り出す
            case validate_one(Kvc) of
                skip ->
                    validate(LastLineNumber, Configurations, KvcList);
                {ok, ValidatedValue} ->
                    ok = kvconf:set_value(Key, ValidatedValue),
                    validate(LastLineNumber, Configurations, KvcList);
                Reason when is_atom(Reason) ->
                    %% 設定には存在しないので最後の行番号を入れる。
                    %% ファイルを最後まで探したけど駄目だった、という気持ち。
                    {error, {Reason, Key, LastLineNumber}}
            end;
        {Value, Line, LineNumber} ->
            case validate_one(Kvc, Value) of
                {ok, ValidatedValue} ->
                    ok = kvconf:set_value(Key, ValidatedValue),
                    validate(LastLineNumber, Configurations, KvcList);
                Reason when is_atom(Reason) ->
                    {error, {Reason, Line, LineNumber}}
            end
    end.


validate_one(#kvc{required = true}) ->
    %% 値が必要なのに key がなかったのでエラー
    missing_required_key;
validate_one(#kvc{required = false, default = undefined}) ->
    %% デフォルト値がない場合は何もしない
    %% 取り出したときに not_found が返ってくるのでそれを処理すること
    skip;
validate_one(#kvc{
               required = false,
               type = Type,
               default = Value
              }) ->
    %% デフォルトも型チェックする
    validate_type(Type, Value).


validate_one(#kvc{required = true, type = Type}, Value) ->
    validate_type(Type, Value);
validate_one(#kvc{required = false, type = Type}, Value) ->
    validate_type(Type, Value).


validate_type(#kvc_atom{candidates = Candidates}, Value) ->
    validate_atom(Value, Candidates);
validate_type(#kvc_list_atom{}, Value) ->
    validate_list_atom(Value);
validate_type(#kvc_string{}, Value) ->
    validate_string(Value);
validate_type(#kvc_integer{min = Min, max = Max}, Value) ->
    validate_integer(Value, Min, Max);
validate_type(#kvc_float{min = Min, max = Max}, Value) ->
    validate_float(Value, Min, Max);
validate_type(#kvc_list_ipv4_address{}, Value) ->
    validate_list_ipv4_address(Value);
validate_type(#kvc_list_ipv6_address{}, Value) ->
    validate_list_ipv6_address(Value);
validate_type(#kvc_ipv4_address{}, Value) ->
    validate_ipv4_address(Value);
validate_type(#kvc_ipv6_address{}, Value) ->
    validate_ipv6_address(Value);
validate_type(#kvc_port_number{}, Value) ->
    validate_port_number(Value);
validate_type(#kvc_boolean{}, Value) ->
    validate_boolean(Value);
validate_type(#kvc_http_uri{}, Value) ->
    validate_http_uri(Value);
validate_type(#kvc_interval{} = Kvc, Value) ->
    validate_interval(Value, Kvc);
validate_type(#kvc_pkix_fullchain_pem_file{}, Value) ->
    kvconf_pkix:validate_pkix_fullchain_pem_file(Value);
validate_type(#kvc_pkix_privkey_pem_file{}, Value) ->
    kvconf_pkix:validate_pkix_privkey_pem_file(Value);
validate_type(#kvc_pkix_cert_pem_file{}, Value) ->
    kvconf_pkix:validate_pkix_cert_pem_file(Value).


validate_atom(_Value, []) ->
    invalid_value;

%% default チェックの枝
validate_atom(Value, Candidates) when is_atom(Value) ->
    F = fun({_, Candidate}) when Candidate =:= Value ->
                true;
           (Candidate) when Candidate =:= Value ->
                true;
           (_) ->
                false
        end,
    case lists:any(F, Candidates) of
        true ->
            {ok, Value};
        _ ->
            invalid_value
    end;
validate_atom(Value, [{Value, Candidate} | _Candidates]) when is_atom(Candidate) ->
    {ok, Candidate};
validate_atom(Value, [Candidate | Candidates]) when is_atom(Candidate) ->
    case atom_to_binary(Candidate, utf8) of
        Value ->
            {ok, Candidate};
        _ ->
            validate_atom(Value, Candidates)
    end;
validate_atom(Value, [_ | Candidates]) ->
    validate_atom(Value, Candidates).


validate_list_atom(Value) when is_binary(Value) ->
    RawListAtom = binary:split(Value, [<<",">>, <<$\s>>], [trim_all, global]),
    validate_list_atom(RawListAtom);
%% デフォルトチェック
validate_list_atom(Value) when is_list(Value) ->
    F = fun(V) when is_atom(V) ->
                V;
           (V) when is_binary(V) ->
                binary_to_atom(V, utf8)
        end,
    {ok, lists:map(F, Value)}.


validate_port_number(Value) ->
    validate_integer(Value, 0, 65535).


%% default チェックの枝
-spec validate_boolean(boolean() | binary()) -> {ok, boolean()} | invalid_value.
validate_boolean(true) ->
    {ok, true};
validate_boolean(false) ->
    {ok, false};
validate_boolean(<<"true">>) ->
    {ok, true};
validate_boolean(<<"false">>) ->
    {ok, false};
validate_boolean(_) ->
    invalid_value.


validate_integer(Value, Min, Max) when is_binary(Value) ->
    try
        IntValue = binary_to_integer(Value),
        validate_integer(IntValue, Min, Max)
    catch
        error:badarg ->
            invalid_value
    end;
validate_integer(Value, Min, infinity) when is_integer(Value) andalso Min =< Value ->
    {ok, Value};
validate_integer(Value, Min, Max) when is_integer(Value) andalso Min =< Value andalso Value =< Max ->
    {ok, Value};
validate_integer(_Value, _Min, _Max) ->
    invalid_value.


validate_float(Value, Min, Max) when is_binary(Value) ->
    try
        IntValue = binary_to_float(Value),
        validate_float(IntValue, Min, Max)
    catch
        error:badarg ->
            invalid_value
    end;
validate_float(Value, Min, infinity) when is_float(Value) andalso Min =< Value ->
    {ok, Value};
validate_float(Value, Min, Max) when is_float(Value) andalso Min =< Value andalso Value =< Max ->
    {ok, Value};
validate_float(_Value, _Min, _Max) ->
    invalid_value.


validate_ipv4_address(Value0) when is_tuple(Value0) ->
    %% これは {1,2,3,4444} とかも通してしまうのでちゃんとチェックする
    case inet:ntoa(Value0) of
        {error, einval} ->
            invalid_value;
        Value ->
            validate_ipv4_address(list_to_binary(Value))
    end;
validate_ipv4_address(Value) ->
    case inet:parse_ipv4strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_ipv6_address(Value0) when is_tuple(Value0) ->
    case inet:ntoa(Value0) of
        {error, einval} ->
            invalid_value;
        Value ->
            validate_ipv6_address(list_to_binary(Value))
    end;
validate_ipv6_address(Value) ->
    case inet:parse_ipv6strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_list_ipv4_address(Value) when is_binary(Value) ->
    RawListIpAddress = binary:split(Value, [<<",">>, <<$\s>>], [trim_all, global]),
    validate_list_ipv4_address0(RawListIpAddress, []);
%% デフォルトチェック
validate_list_ipv4_address(Value) when is_list(Value) ->
    F = fun(IpAddress) ->
                case validate_ipv4_address(IpAddress) of
                    invalid_value ->
                        false;
                    _ ->
                        true
                end
        end,
    case lists:all(F, Value) of
        true ->
            {ok, Value};
        false ->
            invalid_value
    end.


validate_list_ipv4_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address0([Value | Rest], Acc) ->
    case validate_ipv4_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv4_address0(Rest, [IpAddress | Acc]);
        invalid_value ->
            invalid_value
    end.


validate_list_ipv6_address(Value) when is_binary(Value) ->
    RawListIpAddress = binary:split(Value, [<<",">>, <<$\s>>], [trim_all, global]),
    validate_list_ipv6_address0(RawListIpAddress, []);
%% デフォルトチェック
validate_list_ipv6_address(Value) when is_list(Value) ->
    F = fun(IpAddress) ->
                case validate_ipv6_address(IpAddress) of
                    invalid_value ->
                        false;
                    _ ->
                        true
                end
        end,
    case lists:all(F, Value) of
        true ->
            {ok, Value};
        false ->
            invalid_value
    end.


validate_list_ipv6_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv6_address0([Value | Rest], Acc) ->
    case validate_ipv6_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv6_address0(Rest, [IpAddress | Acc]);
        invalid_value ->
            invalid_value
    end.


validate_string(Value) when is_binary(Value) ->
    {ok, Value};
validate_string(_Value) ->
    invalid_value.


validate_http_uri(Value) ->
    case uri_string:parse(Value) of
        #{scheme := Scheme}
          when Scheme =:= <<"https">>;
               Scheme =:= <<"http">> ->
            {ok, Value};
        _ ->
            invalid_value
    end.


-define(IN_TIME_UNIT, [ms, s, min, h]).


%% #kvc_interval{min = {10, ms} , max = {1, sec}, out_unit = millisecond}
-spec validate_interval({non_neg_integer(), kvconf:in_time_unit()} | binary(),
                        #kvc_interval{}) -> {ok, non_neg_integer()} | invalid_value.
%% デフォルト値は binary ではなく {Value, InUnit} になるのでこの枝が必要になる
validate_interval({Value, InUnit},
                  #kvc_interval{
                    min = Min,
                    max = Max,
                    out_time_unit = OutUnit,
                    available_time_units = AvailableTimeUnits
                   }) when is_integer(Value) andalso is_atom(InUnit) ->
    maybe
        true ?= lists:member(InUnit, ?IN_TIME_UNIT),
        ok ?= validate_interval_min({Value, InUnit}, Min),
        ok ?= validate_interval_max({Value, InUnit}, Max),
        ok ?= validate_available_time_unit(InUnit, AvailableTimeUnits),
        validate_interval_out_unit({Value, InUnit}, OutUnit)
    else
        false ->
            invalid_value;
        error ->
            invalid_value
    end;
validate_interval(Value, #kvc_interval{} = Kvc) when is_binary(Value) ->
    case binary:split(Value, [<<$\s>>], [global, trim_all]) of
        [RawInteger0, RawInUnit] ->
            try
                InUnit = binary_to_existing_atom(RawInUnit),
                %% 1_000_000 を 1000000 に変換する
                RawInteger = binary:replace(RawInteger0, <<"_">>, <<>>, [global]),
                Integer = binary_to_integer(RawInteger),
                validate_interval({Integer, InUnit}, Kvc)
            catch
                error:badarg ->
                    invalid_value
            end;
        _ ->
            invalid_value
    end.


validate_interval_min({Value0, InUnit0}, {Min0, MinUnit0}) ->
    {Value, InUnit} = time_unit({Value0, InUnit0}),
    {Min, MinUnit} = time_unit({Min0, MinUnit0}),
    case erlang:convert_time_unit(Value, InUnit, MinUnit) of
        ConvertedValue when Min =< ConvertedValue ->
            ok;
        _ ->
            error
    end.


validate_interval_max({_Value0, _InUnit0}, infinity) ->
    ok;
validate_interval_max({Value0, InUnit0}, {Max0, MaxUnit0}) ->
    {Value, InUnit} = time_unit({Value0, InUnit0}),
    {Max, MaxUnit} = time_unit({Max0, MaxUnit0}),
    case erlang:convert_time_unit(Value, InUnit, MaxUnit) of
        ConvertedValue when ConvertedValue =< Max ->
            ok;
        _ ->
            error
    end.


validate_interval_out_unit({Value0, InUnit0}, OutUnit) ->
    {Value, InUnit} = time_unit({Value0, InUnit0}),
    {ok, erlang:convert_time_unit(Value, InUnit, OutUnit)}.


time_unit({Integer, us}) ->
    {Integer, microsecond};
time_unit({Integer, ms}) ->
    {Integer, millisecond};
time_unit({Integer, s}) ->
    {Integer, second};
time_unit({Integer, min}) ->
    {Integer * 60, second};
time_unit({Integer, h}) ->
    {Integer * 60 * 60, second}.


-spec validate_available_time_unit(kv_conf:in_time_unit(),
                                   undefined | [kv_conf:in_time_unit()]) ->
          ok |
          error.
validate_available_time_unit(_InUnit, undefined) ->
    ok;
validate_available_time_unit(InUnit, AvailableTimeUnits) when is_list(AvailableTimeUnits) ->
    case lists:member(InUnit, AvailableTimeUnits) of
        true ->
            ok;
        false ->
            error
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


validate_interval_test() ->
    ?assertEqual(invalid_value,
                 validate_interval(<<"120 s">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {1, min},
                                     out_time_unit = millisecond
                                    })),

    ?assertEqual({ok, 120_000},
                 validate_interval(<<"1__2__0__0__0__0 ms">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {2, min},
                                     out_time_unit = millisecond
                                    })),

    ?assertEqual({ok, 120_000},
                 validate_interval(<<"120 s">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {2, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual({ok, 120},
                 validate_interval(<<"120 ms">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {2, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual({ok, 7_200_000},
                 validate_interval(<<"120 min">>,
                                   #kvc_interval{
                                     min = {100, min},
                                     max = {120, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual(invalid_value,
                 validate_interval(<<"120">>,
                                   #kvc_interval{
                                     min = {121, min},
                                     max = {130, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual(invalid_value,
                 validate_interval(<<"120 min">>,
                                   #kvc_interval{
                                     min = {100, min},
                                     max = {119, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual({ok, 432_000},
                 validate_interval(<<"120 h">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {120, h},
                                     out_time_unit = second
                                    })),

    %% 数値と単位の間にスペースがないのでエラー
    ?assertEqual(invalid_value,
                 validate_interval(<<"120s">>,
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {2, min},
                                     out_time_unit = millisecond
                                    })),

    %% default テスト
    ?assertEqual({ok, 7_200_000},
                 validate_interval({120, min},
                                   #kvc_interval{
                                     min = {100, min},
                                     max = {120, min},
                                     out_time_unit = millisecond
                                    })),
    ?assertEqual({ok, 432_000},
                 validate_interval({120, h},
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = {120, h},
                                     out_time_unit = second
                                    })),
    ?assertEqual(invalid_value,
                 validate_interval({120, min},
                                   #kvc_interval{
                                     min = {100, min},
                                     max = {119, min},
                                     out_time_unit = millisecond
                                    })),

    %% infinity
    ?assertEqual({ok, 1_800_000},
                 validate_interval({500, h},
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = infinity,
                                     out_time_unit = second
                                    })),

    %% h のみで h 指定してるのでよし
    ?assertEqual({ok, 1_800_000},
                 validate_interval({500, h},
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = infinity,
                                     out_time_unit = second,
                                     available_time_units = [h]
                                    })),

    %% h のみなのに min を指定していてエラー
    ?assertEqual(invalid_value,
                 validate_interval({500, min},
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = infinity,
                                     out_time_unit = second,
                                     available_time_units = [h]
                                    })),

    %% h, min のどちらかなのに s を指定していてエラー
    ?assertEqual(invalid_value,
                 validate_interval({500, s},
                                   #kvc_interval{
                                     min = {0, ms},
                                     max = infinity,
                                     out_time_unit = second,
                                     available_time_units = [h, min]
                                    })),

    ok.


validate_atom_test() ->
    ?assertEqual(invalid_value, validate_atom(<<"b">>, [a])),
    ?assertEqual({ok, a}, validate_atom(a, [a])),
    ok.


validate_list_atom_test() ->
    ?assertEqual({ok, [a, a]}, validate_list_atom([<<"a">>, a])),
    ?assertEqual({ok, [a, b, c]}, validate_list_atom(<<"a, b, c">>)),
    ?assertEqual({ok, [a, b, c]}, validate_list_atom(<<"a,b,c">>)),
    ?assertEqual({ok, [a, b, c]}, validate_list_atom(<<"a, b,       c">>)),
    ok.


validate_integer_test() ->
    ?assertEqual(invalid_value, validate_integer(<<>>, 0, 10)),
    ok.


validate_ipv4_address_test() ->
    ?assertEqual({ok, {1, 2, 3, 4}}, validate_ipv4_address({1, 2, 3, 4})),
    ok.


validate_ipv6_address_test() ->
    ?assertEqual(invalid_value, validate_ipv6_address({1, 2, 3, 4})),
    ?assertEqual({ok, {1, 2, 3, 4, 1, 2, 3, 4}}, validate_ipv6_address({1, 2, 3, 4, 1, 2, 3, 4})),
    ok.


validate_list_ipv4_address_test() ->
    ?assertEqual(invalid_value, validate_list_ipv4_address([{1, 2, 3}])),
    ?assertEqual(invalid_value, validate_list_ipv4_address([{1, 2, 3, 4}, {1, 2, 3}])),
    ?assertEqual({ok, [{1, 2, 3, 4}]}, validate_list_ipv4_address([{1, 2, 3, 4}])),
    ok.


validate_list_ipv6_address_test() ->
    ?assertEqual(invalid_value, validate_list_ipv6_address([{1, 2, 3, 4, 1, 2, 3}])),
    ?assertEqual(invalid_value, validate_list_ipv6_address([{1, 2, 3, 4, 1, 2, 3}, {1, 2, 3}])),
    ?assertEqual({ok, [{1, 2, 3, 4, 1, 2, 3, 4}]}, validate_list_ipv6_address([{1, 2, 3, 4, 1, 2, 3, 4}])),
    ok.


validate_one_test() ->
    ?assertEqual({ok, foo},
                 validate_one(#kvc{
                                key = atom_default,
                                type = #kvc_atom{candidates = [foo, bar]},
                                default = foo
                               })),
    ok.


-endif.

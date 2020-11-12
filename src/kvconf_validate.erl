-module(kvconf_validate).

-export([validate/3]).

-include("kvconf.hrl").


-spec validate(non_neg_integer(), map(), [#kvc{}]) ->
    ok | {error, {atom(), any(), non_neg_integer()}}.
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
validate_one(#kvc{required = false, type = Type, default = Value}) ->
    %% デフォルトも型チェックする
    validate_type(Type, Value).


validate_one(#kvc{required = true, type = Type}, Value) ->
    validate_type(Type, Value);
validate_one(#kvc{required = false, type = Type}, Value) ->
    validate_type(Type, Value).


validate_type(#kvc_atom{candidates = Candidates}, Value) ->
    validate_atom(Value, Candidates);
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
validate_type(#kvc_interval{min = Min, max = Max, out_time_unit = Unit}, Value) ->
    validate_interval(Value, Min, Max, Unit).


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
validate_integer(Value, Min, infinity)
  when is_integer(Value) andalso Min =< Value ->
    {ok, Value};
validate_integer(Value, Min, Max)
  when is_integer(Value) andalso Min =< Value andalso Value =< Max ->
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
validate_float(Value, Min, infinity)
  when is_float(Value) andalso Min =< Value ->
    {ok, Value};
validate_float(Value, Min, Max)
  when is_float(Value) andalso Min =< Value andalso Value =< Max ->
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
validate_list_ipv4_address0([Value|Rest], Acc) ->
    case validate_ipv4_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv4_address0(Rest, [IpAddress|Acc]);
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
validate_list_ipv6_address0([Value|Rest], Acc) ->
    case validate_ipv6_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv6_address0(Rest, [IpAddress|Acc]);
        invalid_value ->
            invalid_value
    end.


validate_string(Value) when is_binary(Value) ->
    {ok, Value};
validate_string(_Value) ->
    invalid_value.


validate_http_uri(Value) ->
    case uri_string:parse(Value) of
        #{scheme := Scheme} when Scheme =:= <<"https">>;
                                 Scheme =:= <<"http">> ->
            {ok, Value};
        _ ->
            invalid_value
    end.


-define(IN_TIME_UNIT, [ms, s, min, h]).

%% TODO(v): infinity 対応
%% #kvc_interval{min = {10, ms} , max = {1, sec}, out_unit = millisecond}
-spec validate_interval({non_neg_integer(), kvconf:in_time_unit()} | binary(),
                        {non_neg_integer(), kvconf:in_time_unit()},
                        {non_neg_integer(), kvconf:in_time_unit()},
                        kvconf:out_time_unit()) ->
    {ok, non_neg_integer()} | invalid_value.
validate_interval({Value, InUnit}, Min, Max, OutUnit)
  when is_integer(Value) andalso is_atom(InUnit) ->
    case validate_interval_min({Value, InUnit}, Min) of
        ok ->
            case validate_interval_max({Value, InUnit}, Max) of
                ok ->
                    validate_interval_out_unit({Value, InUnit}, OutUnit);
                error ->
                    invalid_value
            end;
        error ->
            invalid_value
    end;
validate_interval(Value, Min, Max, OutUnit) when is_binary(Value) ->
    case binary:split(Value, [<<$\s>>], [global, trim_all]) of
        [RawInteger0, RawInUnit] ->
            try
                InUnit = binary_to_existing_atom(RawInUnit),
                case lists:member(InUnit, ?IN_TIME_UNIT) of
                    true ->
                        %% 1_000_000 を 1000000 に変換する
                        RawInteger = binary:replace(RawInteger0, <<"_">>, <<>>, [global]),
                        Integer = binary_to_integer(RawInteger),
                        case validate_interval_min({Integer, InUnit}, Min) of
                            ok ->
                                case validate_interval_max({Integer, InUnit}, Max) of
                                    ok ->
                                        validate_interval_out_unit({Integer, InUnit}, OutUnit);
                                    error ->
                                        invalid_value
                                end;
                            error ->
                                invalid_value
                        end;
                    false ->
                        invalid_value
                end
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


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


validate_interval_test() ->
    ?assertEqual(invalid_value,
                 validate_interval(<<"120 s">>, {0, ms}, {1, min}, millisecond)),
    ?assertEqual({ok, 120_000},
                 validate_interval(<<"120 s">>, {0, ms}, {2, min}, millisecond)),
    ?assertEqual({ok, 120},
                 validate_interval(<<"120 ms">>, {0, ms}, {2, min}, millisecond)),
    ?assertEqual({ok, 7_200_000},
                 validate_interval(<<"120 min">>, {100, min}, {120, min}, millisecond)),
    ?assertEqual(invalid_value,
                 validate_interval(<<"120">>, {121, min}, {130, min}, millisecond)),
    ?assertEqual(invalid_value,
                 validate_interval(<<"120 min">>, {100, min}, {119, min}, millisecond)),
    ?assertEqual({ok, 432_000},
                 validate_interval(<<"120 h">>, {0, ms}, {120, h}, second)),

    %% 数値と単位の間にスペースがないのでエラー
    ?assertEqual(invalid_value,
                 validate_interval(<<"120s">>, {0, ms}, {2, min}, millisecond)),

    %% default テスト
    ?assertEqual({ok, 7_200_000},
                 validate_interval({120, min}, {100, min}, {120, min}, millisecond)),
    ?assertEqual({ok, 432_000},
                 validate_interval({120, h}, {0, ms}, {120, h}, second)),
    ?assertEqual(invalid_value,
                 validate_interval({120, min}, {100, min}, {119, min}, millisecond)),

    ok.


validate_atom_test() ->
    ?assertEqual(invalid_value, validate_atom(<<"b">>, [a])),
    ?assertEqual({ok, a}, validate_atom(a, [a])),
    ok.


validate_integer_test() ->
    ?assertEqual(invalid_value, validate_integer(<<>>, 0, 10)),
    ok.


validate_ipv4_address_test() ->
    ?assertEqual({ok, {1,2,3,4}}, validate_ipv4_address({1,2,3,4})),
    ok.


validate_ipv6_address_test() ->
    ?assertEqual(invalid_value, validate_ipv6_address({1,2,3,4})),
    ?assertEqual({ok, {1,2,3,4,1,2,3,4}}, validate_ipv6_address({1,2,3,4,1,2,3,4})),
    ok.



validate_list_ipv4_address_test() ->
    ?assertEqual(invalid_value, validate_list_ipv4_address([{1,2,3}])),
    ?assertEqual(invalid_value, validate_list_ipv4_address([{1,2,3,4}, {1,2,3}])),
    ?assertEqual({ok, [{1,2,3,4}]}, validate_list_ipv4_address([{1,2,3,4}])),
    ok.


validate_list_ipv6_address_test() ->
    ?assertEqual(invalid_value, validate_list_ipv6_address([{1,2,3,4,1,2,3}])),
    ?assertEqual(invalid_value, validate_list_ipv6_address([{1,2,3,4,1,2,3}, {1,2,3}])),
    ?assertEqual({ok, [{1,2,3,4,1,2,3,4}]}, validate_list_ipv6_address([{1,2,3,4,1,2,3,4}])),
    ok.



validate_one_test() ->
    ?assertEqual({ok, foo},
                 validate_one(#kvc{key = atom_default, type = #kvc_atom{candidates = [foo, bar]}, default = foo})),
    ok.


-endif.

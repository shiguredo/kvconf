-module(kvconf_validate).

-export([validate/3]).

-include("kvconf.hrl").

-include_lib("eunit/include/eunit.hrl").


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
    skip;
validate_one(#kvc{required = false, default = Value}) ->
    %% デフォルトの場合はそのまま返してしまう
    {ok, Value}.


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
validate_type(_UnknownType, _Value) ->
    unknown_type.


validate_atom(_Value, []) ->
    invalid_value;
validate_atom(Value, [Candidate | Candidates]) when is_atom(Candidate) ->
    case atom_to_binary(Candidate, utf8) of
        Value ->
            {ok, Candidate};
        _ ->
            validate_atom(Value, Candidates)
    end;
validate_atom(Value, [{Value, Candidate} | _Candidates]) when is_atom(Candidate) ->
    {ok, Candidate};
validate_atom(Value, [_ | Candidates]) ->
    validate_atom(Value, Candidates).


validate_port_number(Value) ->
    validate_integer(Value, 0, 65535).


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


validate_ipv4_address(Value) ->
    case inet:parse_ipv4strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_list_ipv4_address(Value) ->
    RawListIpAddress = binary:split(Value, [<<",">>, <<$\s>>], [trim_all, global]),
    validate_list_ipv4_address0(RawListIpAddress, []).

validate_list_ipv4_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address0([Value|Rest], Acc) ->
    case validate_ipv4_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv4_address0(Rest, [IpAddress|Acc]);
        invalid_value ->
            invalid_value
    end.


validate_list_ipv6_address(Value) ->
    RawListIpAddress = binary:split(Value, [<<",">>, <<$\s>>], [trim_all, global]),
    validate_list_ipv6_address0(RawListIpAddress, []).

validate_list_ipv6_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv6_address0([Value|Rest], Acc) ->
    case validate_ipv6_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv6_address0(Rest, [IpAddress|Acc]);
        invalid_value ->
            invalid_value
    end.



validate_ipv6_address(Value) ->
    case inet:parse_ipv6strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
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


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

validate_integer_test() ->
    ?assertEqual(invalid_value, validate_integer(<<>>, 0, 10)),
    ok.


validate_one_test() ->
    ?assertEqual({ok, foo},
                 validate_one(#kvc{key = atom_default, type = #kvc_atom{candidates = [foo, bar]}, default = foo})),
    ok.


-endif.

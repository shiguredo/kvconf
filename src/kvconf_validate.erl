-module(kvconf_validate).

-export([validate/3]).

-include("kvconf.hrl").


-spec validate(non_neg_integer(), map(), [definition_internal()]) ->
                      ok | {error, {atom(), any(), non_neg_integer()}}.
validate(_LastLineNumber, _Configurations, []) ->
    ok;
validate(LastLineNumber, Configurations, [{Key, Type, Requirement} | DefinitionList]) ->
    case maps:get(atom_to_binary(Key, utf8), Configurations, undefined) of
        undefined ->
            case validate_one(Type, Requirement, undefined) of
                {ok, ValidatedValue} ->
                    ok = kvconf:set_value(Key, ValidatedValue),
                    validate(LastLineNumber, Configurations, DefinitionList);
                {error, Reason} ->
                    %% 設定には存在しないので最後の行番号を入れる。
                    %% ファイルを最後まで探したけど駄目だった、という気持ち。
                    {error, {Reason, Key, LastLineNumber}}
            end;
        {Value, Line, LineNumber} ->
            case validate_one(Type, Requirement, Value) of
                {ok, ValidatedValue} ->
                    ok = kvconf:set_value(Key, ValidatedValue),
                    validate(LastLineNumber, Configurations, DefinitionList);
                {error, Reason} ->
                    {error, {Reason, Line, LineNumber}}
            end
    end.


validate_one(_Type, required, undefined) ->
    {error, missing_required_key};
validate_one(Type, required, Value) ->
    validate_type(Type, Value);
validate_one(_Type, optional, undefined) ->
    %% デフォルト値はそのまま
    {ok, undefined};
validate_one(Type, optional, Value) ->
    validate_type(Type, Value);
validate_one(_Type, {optional, DefaultValue}, undefined) ->
    {ok, DefaultValue};
validate_one(Type, {optional, _DefaultValue}, Value) ->
    validate_type(Type, Value).


validate_type(string, Value) ->
    validate_string(Value);
validate_type({integer, Min, Max}, Value) ->
    validate_integer(Value, Min, Max);
validate_type(ipv4_address, Value) ->
    validate_ipv4_address(Value);
validate_type(ipv6_address, Value) ->
    validate_ipv6_address(Value);
validate_type(ipv4_address_and_port_number, Value) ->
    validate_ipv4_address_and_port_number(Value);
validate_type(port_number, Value) ->
    validate_port_number(Value);
validate_type(boolean, Value) ->
    validate_boolean(Value);
validate_type(http_uri, Value) ->
    validate_http_uri(Value);
validate_type(_UnknownType, _Value) ->
    unknown_type.


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
        throw:badarg ->
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


validate_ipv4_address(Value) ->
    case inet:parse_ipv4strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_ipv6_address(Value) ->
    case inet:parse_ipv6strict_address(binary_to_list(Value)) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_ipv4_address_and_port_number(Value) when is_binary(Value) ->
    case binary:split(Value, <<":">>) of
        [RawIpAddress, RawPort] ->
            case inet:parse_ipv4strict_address(binary_to_list(RawIpAddress)) of
                {ok, IpAddress} ->
                    try binary_to_integer(RawPort) of
                        Port when 0 =< Port andalso Port =< 65535 ->
                            {ok, {IpAddress, Port}};
                        _ ->
                            invalid_value
                    catch
                        _:_ ->
                            invalid_value
                    end;
                {error, _Reason} ->
                    invalid_value
            end;
        _ ->
            invalid_value
    end;
validate_ipv4_address_and_port_number(_Value) ->
    invalid_value.


validate_string(Value) when is_binary(Value) ->
    {ok, Value};
validate_string(_Value) ->
    invalid_value.


validate_http_uri(Value) ->
    case http_uri:parse(Value) of
        {ok, _Result} ->
            {ok, Value};
        {error, _Reason} ->
            invalid_value
    end.

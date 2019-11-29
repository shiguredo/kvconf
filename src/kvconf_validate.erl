-module(kvconf_validate).

-export([validate/2]).

-spec validate(atom(),  [kvconf:definition()]) ->
                      ok | {error, {atom(), any(), non_neg_integer()}}.
validate(_Applicaiton, Configurations, []) ->
    ok;
validate(Application, Configurations, [{Key, Type, required} | Rest]) ->
    case maps:find(atom_to_binary, utf8) of
        undefined ->
            {error, {missing_required_key, Key, 0}};
        {ok, Value} ->
            validate0(Application, Configurations, Rest, Key, Type, Value)
    end;
validate(Application, Configurations, [{Key, Type, optional} | Rest]) ->
    case maps:find(atom_to_binary, utf8) of
        undefined ->
            validate(Application, Configurations, Rest);
        {ok, ValueAndLine} ->
            validate0(Application, Configurations, Rest, Key, Type, ValueAndLine)
    end;
validate(Application, [{Key, Type, optional, Default} | Rest]) ->
    case maps:find(atom_to_binary, utf8) of
        undefined ->
            validate0(Application, Configurations, Rest, Key, Type, {Default, 0})
        {ok, ValueAndLine} ->
            validate0(Application, Configurations, Rest, Key, Type, ValueAndLine)
    end.


validate0(Application, Configurations, Rest, Key, Type, {Value, LineNumber}) ->
    case validate_type(Type, Value) of
        ok ->
            validate(Application, Configurations, Rest);
        {ok, ConvertedValue} ->
            %% 変換して戻ってくる可能性もある
            ok = application:set_env(Application, Key, ConvertedValue),
            validate(Application, Configurations, Rest);
        Reason when is_atom(Reason) ->
            {error, {Reason, Value, LineNumber}}
    end.


validate_type(string, Value) ->
    validate_string(Value);
validate_type(list_string, Value) ->
    validate_list_string(Value);
validate_type({integer, Min, Max}, Value) ->
    validate_integer(Value, Min, Max);
validate_type(ipv4_address, Value) ->
    validate_ipv4_address(Value);
validate_type({list_ipv4_address, Min}, Value) ->
    validate_list_ipv4_address(Value, Min);
validate_type(ipv6_address, Value) ->
    validate_ipv6_address(Value);
validate_type({list_ipv6_address, Min}, Value) ->
    validate_list_ipv6_address(Value, Min);
validate_type(ipv4_address_and_port_number, Value) ->
    validate_ipv4_address_and_port_number(Value);
validate_type(list_ipv4_address_and_port_number, Value) ->
    validate_list_ipv4_address_and_port_number(Value);
validate_type(port_number, Value) ->
    validate_port_number(Value);
validate_type(boolean, Value) ->
    validate_boolean(Value);
validate_type(http_uri, Value) ->
    validate_http_uri(Value);
validate_type(list_http_uri, Value) ->
    validate_list_http_uri(Value);
validate_type(_UnknownType, _Value) ->
    unknown_type.


validate_port_number(Value) ->
    validate_integer(Value, 0, 65535).


validate_boolean(true) ->
    ok;
validate_boolean(false) ->
    ok;
validate_boolean(_) ->
    invalid_value.


validate_integer(Value, Min, infinity)
  when is_integer(Value) andalso Min =< Value ->
    ok;
validate_integer(Value, Min, Max)
  when is_integer(Value) andalso Min =< Value andalso Value =< Max ->
    ok;
validate_integer(_Value, _Min, _Max) ->
    invalid_value.


validate_ipv4_address(Value) ->
    case inet:parse_ipv4strict_address(Value) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_list_ipv4_address(Value, Min) when is_list(Value) andalso length(Value) >= Min ->
    validate_list_ipv4_address0(Value, []);
validate_list_ipv4_address(_Value, _Min) ->
    invalid_value.

validate_list_ipv4_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address0([Value|Rest], Acc) ->
    case validate_ipv4_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv4_address0(Rest, [IpAddress|Acc]);
        invalid_value ->
            invalid_value
    end.


validate_ipv6_address(Value) ->
    case inet:parse_ipv6strict_address(Value) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            invalid_value
    end.


validate_list_ipv6_address(Value, Min) when is_list(Value) andalso length(Value) >= Min ->
    validate_list_ipv6_address0(Value, []);
validate_list_ipv6_address(_Value, _Min) ->
    invalid_value.

validate_list_ipv6_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv6_address0([Value|Rest], Acc) ->
    case validate_ipv6_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv6_address0(Rest, [IpAddress|Acc]);
        invalid_value ->
            invalid_value
    end.



validate_ipv4_address_and_port_number(Value) when is_list(Value) ->
    validate_ipv4_address_and_port_number(list_to_binary(Value));
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


validate_list_ipv4_address_and_port_number(Value) ->
    validate_list_ipv4_address_and_port_number(Value, []).

validate_list_ipv4_address_and_port_number([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address_and_port_number([Value|Rest], Acc) ->
    case validate_ipv4_address_and_port_number(Value) of
        {ok, Host} ->
            validate_list_ipv4_address_and_port_number(Rest, [Host|Acc]);
        _ ->
            invalid_value
    end.


%% FIXME(nakai): 手抜き
validate_string(Value) when is_list(Value) ->
    ok;
validate_string(_Value) ->
    invalid_value.


validate_list_string(Value) when is_list(Value) ->
    F = fun(V) when is_list(V) ->
                true;
           (_V) ->
                false
        end,
    case lists:all(F, Value) of
        true ->
            ok;
        false ->
            invalid_value
    end;
validate_list_string(_Value) ->
    invalid_value.


validate_http_uri(Value) ->
    case http_uri:parse(Value) of
        {ok, _Result} ->
            ok;
        {error, _Reason} ->
            invalid_value
    end.


%% XXX(nakai): 最初から空だったのを許可するか？
    %% まずは空を許可する仕組みで作る
validate_list_http_uri([]) ->
    ok;
validate_list_http_uri([Value|Rest]) ->
    case validate_http_uri(Value) of
        ok ->
            validate_list_http_uri(Rest);
        invalid_value ->
            invalid_value
    end.

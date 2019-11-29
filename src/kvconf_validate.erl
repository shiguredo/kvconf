-module(kvconf_validate).

-export([validate/2]).
-export([format_error/1]).

-type key() :: atom().
-type type() :: string | list_string | {integer, integer(), integer()} |
                ipv4_address | list_ipv4_address | ipv6_address | list_ipv6_address |
                host | port_number | boolean | http_uri | list_http_uri | list_to_binary.
-type required() :: required | optional.


-spec validate(atom(), [{key(), type(), required()}]) -> ok | {error, term()}.
validate(_Applicaiton, []) ->
    ok;
validate(Application, [{Key, Type, required}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            {error, {required, Key}};
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end;
validate(Application, [{Key, Type, optional}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            validate(Application, Rest);
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end;
validate(Application, [{Key, Type, optional, Default}|Rest]) ->
    case application:get_env(Application, Key) of
        undefined ->
            %% デフォルトの値は間違っていないという前提
            ok = application:set_env(Application, Key, Default),
            validate(Application, Rest);
        {ok, Value} ->
            validate0(Application, Rest, Key, Type, Value)
    end.


validate0(Application, Rest, Key, Type, Value) ->
    case validate_type(Type, Value) of
        ok ->
            validate(Application, Rest);
        {ok, ConvertValue} ->
            %% 変換して戻ってくる可能性もある
            ok = application:set_env(Application, Key, ConvertValue),
            validate(Application, Rest);
        Reason when is_atom(Reason) ->
            {error, {Reason, Key, Type, Value}}
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
    badarg.


validate_integer(Value, Min, infinity)
  when is_integer(Value) andalso Min =< Value ->
    ok;
validate_integer(Value, Min, Max)
  when is_integer(Value) andalso Min =< Value andalso Value =< Max ->
    ok;
validate_integer(_Value, _Min, _Max) ->
    badarg.


validate_ipv4_address(Value) ->
    case inet:parse_ipv4strict_address(Value) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            badarg
    end.


validate_list_ipv4_address(Value, Min) when is_list(Value) andalso length(Value) >= Min ->
    validate_list_ipv4_address0(Value, []);
validate_list_ipv4_address(_Value, _Min) ->
    badarg.

validate_list_ipv4_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address0([Value|Rest], Acc) ->
    case validate_ipv4_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv4_address0(Rest, [IpAddress|Acc]);
        badarg ->
            badarg
    end.


validate_ipv6_address(Value) ->
    case inet:parse_ipv6strict_address(Value) of
        {ok, IpAddress} ->
            {ok, IpAddress};
        {error, _Reason} ->
            badarg
    end.


validate_list_ipv6_address(Value, Min) when is_list(Value) andalso length(Value) >= Min ->
    validate_list_ipv6_address0(Value, []);
validate_list_ipv6_address(_Value, _Min) ->
    badarg.

validate_list_ipv6_address0([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv6_address0([Value|Rest], Acc) ->
    case validate_ipv6_address(Value) of
        {ok, IpAddress} ->
            validate_list_ipv6_address0(Rest, [IpAddress|Acc]);
        badarg ->
            badarg
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
                            badarg
                    catch
                        _:_ ->
                            badarg
                    end;
                {error, _Reason} ->
                    badarg
            end;
        _ ->
            badarg
    end;
validate_ipv4_address_and_port_number(_Value) ->
    badarg.


validate_list_ipv4_address_and_port_number(Value) ->
    validate_list_ipv4_address_and_port_number(Value, []).

validate_list_ipv4_address_and_port_number([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_list_ipv4_address_and_port_number([Value|Rest], Acc) ->
    case validate_ipv4_address_and_port_number(Value) of
        {ok, Host} ->
            validate_list_ipv4_address_and_port_number(Rest, [Host|Acc]);
        _ ->
            badarg
    end.


%% FIXME(nakai): 手抜き
validate_string(Value) when is_list(Value) ->
    ok;
validate_string(_Value) ->
    badarg.


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
            badarg
    end;
validate_list_string(_Value) ->
    badarg.


validate_http_uri(Value) ->
    case http_uri:parse(Value) of
        {ok, _Result} ->
            ok;
        {error, _Reason} ->
            badarg
    end.


%% XXX(nakai): 最初から空だったのを許可するか？
    %% まずは空を許可する仕組みで作る
validate_list_http_uri([]) ->
    ok;
validate_list_http_uri([Value|Rest]) ->
    case validate_http_uri(Value) of
        ok ->
            validate_list_http_uri(Rest);
        badarg ->
            badarg
    end.


-spec format_error(term()) -> string().
format_error({unknown_type, Type, Value}) ->
    io_lib:format("CONFIG-BAD-VALUE | type=~s, value=~p", [Type, Value]);
format_error({badarg, Key, _Type, Value}) ->
    io_lib:format("CONFIG-BAD-VALUE | key=~s, value=~p", [Key, Value]);
format_error({required, Key}) ->
    io_lib:format("CONFIG-REQUIRED | key=~s", [Key]).

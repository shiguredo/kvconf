-module(kvconf).

-export([initialize/2, initialize/3]).
-export([set_value/2,
         unset_value/1,
         get_value/1]).

-export_type([key/0,
              type/0]).
-export_type([in_time_unit/0,
              out_time_unit/0]).

-include("kvconf.hrl").

-type key() :: atom().
-type type() :: #kvc_atom{} |
                #kvc_list_atom{} |
                #kvc_string{} |
                #kvc_list_string{} |
                #kvc_integer{} |
                #kvc_float{} |
                #kvc_boolean{} |
                #kvc_ipv4_address{} |
                #kvc_ipv6_address{} |
                #kvc_list_ipv4_address{} |
                #kvc_list_ipv6_address{} |
                #kvc_port_number{} |
                #kvc_http_uri{} |
                #kvc_interval{} |
                #kvc_pkix_fullchain_pem_file{} |
                #kvc_pkix_privkey_pem_file{} |
                #kvc_pkix_cert_pem_file{}.

%% 入力を許可する値の単位
-type in_time_unit() :: ms | s | min | h.
-type out_time_unit() :: second | millisecond | microsecond.


-spec initialize([#kvc{}], binary()) ->
          {ok, [binary()], [{atom(), term()}]} |
          {error, term()}.
initialize(KvcList, Binary) ->
    initialize(KvcList, Binary, #{}).


-spec initialize([#kvc{}], binary(), map()) ->
          {ok, [binary()], [{atom(), term()}]} |
          {error, term()}.
initialize(KvcList, Binary, Options) ->
    maybe
        ok ?= validate_options(Options),
        {ok, Configurations0, LastLineNumber} ?= parse(Binary),
        Configurations = maybe_env_overrides(Configurations0, KvcList, Options),
        ok ?= kvconf_validate:validate(LastLineNumber, Configurations, KvcList),
        UnknownKeys = unknown_keys(Configurations, KvcList),
        %% undoc_ で設定された値一覧を返す
        UndocKvList = undoc_kv_list(Configurations, KvcList),
        {ok, UnknownKeys, UndocKvList}
    else
        {error, Reason} ->
            {error, Reason}
    end.


%% Options のバリデーション
-spec validate_options(map()) -> ok | {error, term()}.
validate_options(Options) when map_size(Options) =:= 0 ->
    ok;
validate_options(#{env_prefix := Value} = Options0) ->
    case is_binary(Value) of
        %% 空バイナリは許容しない
        true when byte_size(Value) > 0 ->
            Options = maps:remove(env_prefix, Options0),
            validate_options(Options);
        _ ->
            {error, {invalid_option_value, env_prefix, Value}}
    end;
validate_options(Options) ->
    %% 見知らぬキー一覧はここにくる
    {error, {unknown_option_keys, maps:keys(Options)}}.


-spec maybe_env_overrides(map(), [#kvc{}], map()) -> map().
maybe_env_overrides(Configurations, KvcList, Options) ->
    case maps:get(env_prefix, Options, undefined) of
        undefined ->
            %% prefix が無い場合は環境変数による上書きを行わない
            Configurations;
        Prefix ->
            %% 環境変数が存在する場合は設定を上書き
            maybe_env_overrides0(Configurations, KvcList, Prefix)
    end.


maybe_env_overrides0(Configurations, [], _Prefix) ->
    Configurations;
maybe_env_overrides0(Configurations, [#kvc{key = Key} | Rest], Prefix) ->
    EnvName = key_to_env_name(Key, Prefix),
    %% os:getenv/1 が string のみを要求している
    case os:getenv(EnvName) of
        false ->
            maybe_env_overrides0(Configurations, Rest, Prefix);
        EnvValue ->
            %% 環境変数の値で上書き
            %% Line には環境変数名を、LineNumber には 0 を設定
            BinKey = atom_to_binary(Key, utf8),
            BinValue = list_to_binary(EnvValue),
            Line = list_to_binary("ENV:" ++ EnvName),
            NewConfigurations = Configurations#{BinKey => {BinValue, Line, 0}},
            maybe_env_overrides0(NewConfigurations, Rest, Prefix)
    end.


%% キーから環境変数名への変換
-spec key_to_env_name(atom(), binary() | undefined) -> string().
key_to_env_name(Key, undefined) ->
    %% Prefix なし
    KeyStr = atom_to_binary(Key),
    %% os:getenv/1 が string のみを要求しているので変換
    binary_to_list(string:uppercase(KeyStr));
key_to_env_name(Key, Prefix) ->
    %% Prefix あり
    PrefixStr = string:uppercase(Prefix),
    KeyStr = atom_to_binary(Key),
    UpperKeyStr = string:uppercase(KeyStr),
    %% os:getenv/1 が string のみを要求している
    binary_to_list(<<PrefixStr/binary, "_", UpperKeyStr/binary>>).


%% XXX(v): 効率死ぬほど良くない
undoc_kv_list(Configurations, KvcList) ->
    undoc_kv_list(maps:keys(Configurations), KvcList, []).


undoc_kv_list([], _KvcList, Acc) ->
    lists:reverse(Acc);
undoc_kv_list([<<"undoc_", _/binary>> = RawKey | Keys], KvcList, Acc) ->
    Key = binary_to_atom(RawKey),
    case get_value(Key) of
        not_found ->
            %% 知らないキーはスキップ
            undoc_kv_list(Keys, KvcList, Acc);
        Value ->
            undoc_kv_list(Keys, KvcList, [{Key, Value} | Acc])
    end;
undoc_kv_list([_ | Keys], KvcList, Acc) ->
    undoc_kv_list(Keys, KvcList, Acc).


%% XXX(v): 効率死ぬほど良くない
unknown_keys(Configurations, KvcList) ->
    unknown_keys(maps:keys(Configurations), KvcList, []).


unknown_keys([], _KvcList, Acc) ->
    lists:reverse(Acc);
unknown_keys([Key | Keys], KvcList, Acc) ->
    case lists:keyfind(binary_to_atom(Key), #kvc.key, KvcList) of
        false ->
            unknown_keys(Keys, KvcList, [Key | Acc]);
        _ ->
            unknown_keys(Keys, KvcList, Acc)
    end.


-spec set_value(key(), term()) -> ok.
set_value(Key, Value) ->
    ok = persistent_term:put(Key, Value).


-spec unset_value(key()) -> ok.
unset_value(Key) ->
    _ = persistent_term:erase(Key),
    ok.


-spec get_value(key()) -> term().
get_value(Key) ->
    persistent_term:get(Key, not_found).


parse(Binary) ->
    Lines = binary:split(Binary, <<$\n>>, [global]),
    parse_lines(#{}, Lines, 1).


-spec parse_lines(map(), [binary()], integer()) ->
          {ok, map(), integer()} |
          {error,
           {duplicated_key, binary(), integer()} |
           {invalid_line_format, binary(), integer()}}.
parse_lines(Configurations, [], LastLineNumber) ->
    {ok, Configurations, LastLineNumber};
parse_lines(Configurations, [Line | Lines], LineNumber) ->
    case re:run(Line, <<"^ *(#.*)?$">>) of
        %% コメント、空白だけの行はスキップする
        {match, _} ->
            parse_lines(Configurations, Lines, LineNumber + 1);
        nomatch ->
            %% TODO: ここは定数でもいいかもしれない
            case re:run(Line, <<"^([^=]*)=(.*)$">>, [{capture, all, binary}]) of
                nomatch ->
                    {error, {invalid_line_format, Line, LineNumber}};
                {match, [_, RawKey, RawValue]} ->
                    Key = string:trim(RawKey),
                    Value = string:trim(RawValue),
                    case maps:is_key(Key, Configurations) of
                        true ->
                            {error, {duplicated_key, Key, LineNumber}};
                        false ->
                            parse_lines(Configurations#{Key => {Value, Line, LineNumber}},
                                        Lines,
                                        LineNumber + 1)
                    end
            end
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


unknown_keys_test() ->
    ?assertEqual([<<"abc">>], unknown_keys(#{<<"abc">> => a}, [])),

    ?assertEqual([<<"abc">>],
                 unknown_keys(#{<<"two_digits">> => 20, <<"abc">> => b},
                              [#kvc{
                                 key = two_digits,
                                 type = #kvc_integer{min = 10, max = 99},
                                 required = true
                                }])),
    ok.


undoc_kv_list_test() ->
    ok = persistent_term:put(two_digits, 20),
    ok = persistent_term:put(undoc_abc, 30),
    ?assertEqual([{undoc_abc, 30}],
                 undoc_kv_list(#{
                                 <<"two_digits">> => 20,
                                 <<"undoc_abc">> => 30,
                                 <<"undoc_xyz">> => 10
                                },
                               [#kvc{
                                  key = two_digits,
                                  type = #kvc_integer{min = 10, max = 99},
                                  required = false
                                 },
                                #kvc{
                                  key = undoc_abc,
                                  type = #kvc_integer{min = 10, max = 99},
                                  required = false
                                 }])),
    ok.


validate_options_test() ->
    %% 空の Options
    ?assertEqual(ok, validate_options(#{})),

    %% env_prefix が binary の場合
    ?assertEqual(ok, validate_options(#{env_prefix => <<"TEST">>})),

    %% env_prefix が空バイナリの場合
    ?assertEqual({error, {invalid_option_value, env_prefix, <<>>}},
                 validate_options(#{env_prefix => <<>>})),

    %% env_prefix が binary でない場合
    ?assertEqual({error, {invalid_option_value, env_prefix, "TEST"}},
                 validate_options(#{env_prefix => "TEST"})),
    ?assertEqual({error, {invalid_option_value, env_prefix, 123}},
                 validate_options(#{env_prefix => 123})),

    %% 不正なキーが含まれる場合
    ?assertEqual({error, {unknown_option_keys, [invalid_key]}},
                 validate_options(#{invalid_key => <<"value">>})),
    ?assertEqual({error, {unknown_option_keys, [spam, egg]}},
                 validate_options(#{env_prefix => <<"TEST">>, spam => 1, egg => 2})),

    ok.


-endif.

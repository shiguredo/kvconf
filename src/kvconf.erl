-module(kvconf).

-export([initialize/2]).
-export([set_value/2, unset_value/1, get_value/1]).

-export_type([key/0, type/0]).
-export_type([in_time_unit/0, out_time_unit/0]).

-include("kvconf.hrl").

-type key() :: atom().
-type type() :: #kvc_atom{} | #kvc_list_atom{} | #kvc_string{} | #kvc_integer{} | #kvc_float{} |
                #kvc_boolean{} | #kvc_ipv4_address{} | #kvc_ipv6_address{} |
                #kvc_list_ipv4_address{} | #kvc_list_ipv6_address{} |
                #kvc_port_number{} | #kvc_http_uri{} | #kvc_interval{} |
                #kvc_pkix_fullchain_pem_file{} | #kvc_pkix_privkey_pem_file{} | #kvc_pkix_cert_pem_file{}.


%% 入力を許可する値の単位
-type in_time_unit() :: ms | s | min | h.
-type out_time_unit() :: second | millisecond | microsecond.


-spec initialize([#kvc{}], binary()) -> {ok, [binary()], [{atom(), term()}]} |
                                        {error, {atom(), key(), any(), non_neg_integer()}}.
initialize(KvcList, Binary) ->
    case parse(Binary) of
        {ok, Configurations, LastLineNumber} ->
            case kvconf_validate:validate(LastLineNumber, Configurations, KvcList) of
                ok ->
                    UnknownKeys = unknown_keys(Configurations, KvcList),
                    %% undoc_ で設定された値一覧を返す
                    UndocKvList = undoc_kv_list(Configurations, KvcList),
                    {ok, UnknownKeys, UndocKvList};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% XXX(v): 効率死ぬほど良くない
undoc_kv_list(Configurations, KvcList) ->
    undoc_kv_list(maps:keys(Configurations), KvcList, []).


undoc_kv_list([], _KvcList, Acc) ->
    lists:reverse(Acc);
undoc_kv_list([<<"undoc_", _/binary>> = RawKey | Keys],  KvcList, Acc) ->
    Key = binary_to_atom(RawKey),
    case get_value(Key) of
        not_found ->
            %% 知らないキーはスキップ
            undoc_kv_list(Keys, KvcList, Acc);
        Value ->
            undoc_kv_list(Keys, KvcList, [ {Key, Value} | Acc])
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


parse_lines(Configurations, [], LastLineNumber) ->
    {ok, Configurations, LastLineNumber};
parse_lines(Configurations, [Line | Lines], LineNumber) ->
    case re:run(Line, <<"^ *(#.*)?$">>) of
        %% コメント、空白だけの行はスキップする
        {match, _} ->
            parse_lines(Configurations, Lines, LineNumber + 1);
        nomatch ->
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
    ?assertEqual([<<"abc">>],
                 unknown_keys(#{<<"abc">> => a},
                              [])),

    ?assertEqual([<<"abc">>],
                 unknown_keys(#{<<"two_digits">> => 20, <<"abc">> => b},
                              [#kvc{key = two_digits,
                                    type = #kvc_integer{min = 10, max = 99},
                                    required = true}])),
    ok.


undoc_kv_list_test() ->
    ok = persistent_term:put(two_digits, 20),
    ok = persistent_term:put(undoc_abc, 30),
    ?assertEqual([{undoc_abc, 30}],
                 undoc_kv_list(#{<<"two_digits">> => 20,
                                 <<"undoc_abc">> => 30,
                                 <<"undoc_xyz">> => 10},
                               [#kvc{key = two_digits,
                                     type = #kvc_integer{min = 10, max = 99},
                                     required = false},
                                #kvc{key = undoc_abc,
                                     type = #kvc_integer{min = 10, max = 99},
                                     required = false}])),
    ok.


-endif.

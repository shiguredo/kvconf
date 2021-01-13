-module(kvconf).

-export([initialize/2]).
-export([set_value/2, unset_value/1, get_value/1]).

-export_type([key/0, type/0]).
-export_type([in_time_unit/0, out_time_unit/0]).

-include("kvconf.hrl").

-type key() :: atom().
-type type() :: #kvc_atom{} | #kvc_string{} | #kvc_integer{} | #kvc_float{} |
                #kvc_boolean{} | #kvc_ipv4_address{} | #kvc_ipv6_address{} |
                #kvc_list_ipv4_address{} | #kvc_list_ipv6_address{} |
                #kvc_port_number{} | #kvc_http_uri{} | #kvc_interval{}.


%% 入力を許可する値の単位
-type in_time_unit() :: ms | s | min | h.
-type out_time_unit() :: second | millisecond | microsecond.


-spec initialize([#kvc{}], binary()) -> {ok, [binary()]} | {error, {atom(), key(), any(), non_neg_integer()}}.
initialize(KvcList, Binary) ->
    case parse(Binary) of
        {ok, Configurations, LastLineNumber} ->
            case kvconf_validate:validate(LastLineNumber, Configurations, KvcList) of
                ok ->
                    UnknownKeys = unknown_keys(Configurations, KvcList),
                    {ok, UnknownKeys};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


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
                 unknown_keys(#{<<"two_digits">> => a, <<"abc">> => b},
                              [#kvc{key = two_digits,
                                    type = #kvc_integer{min = 10, max = 99},
                                    required = true}])),
    ok.

-endif.

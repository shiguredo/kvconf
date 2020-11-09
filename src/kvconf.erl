-module(kvconf).

-export([initialize/2]).
-export([set_value/2, unset_value/1, get_value/1]).

-include("kvconf.hrl").


-spec initialize([#kvc{}], binary()) -> ok | {error, {atom(), key(), any(), non_neg_integer()}}.
initialize(KvcList, Binary) ->
    case parse(Binary) of
        {ok, Configurations, LastLineNumber} ->
            kvconf_validate:validate(LastLineNumber, Configurations, KvcList);
        {error, Reason} ->
            {error, Reason}
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

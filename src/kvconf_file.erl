-module(kvconf_file).

-export([open/1]).

-spec open(binary()) -> {ok, map()} | {error, {atom(), any(), non_neg_integer()}}.
open(Path) ->
    case file:open(Path, [binary, raw]) of
        {error, Reason} ->
            {error, {open_error, Reason, 0}};
        {ok, File} ->
            parse_lines(File, #{}, 1)
    end.


parse_lines(File, Configurations, LineNumber) ->
    case file:read_line(File) of
        eof ->
            {ok, Configurations};
        {error, Reason} ->
            {error, {read_line_error, Reason, LineNumber}};
        {ok, Line} ->
            case re:run(Line, <<"^ *(#.*)?$">>) of
                %% コメント、空白だけの行はスキップする
                {match, _} ->
                    parse_lines(File, Configurations, LineNumber + 1);
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
                                    parse_lines(File,
                                                Configurations#{Key => {Value, LineNumber}},
                                                LineNumber + 1)
                            end
                    end
            end
    end.

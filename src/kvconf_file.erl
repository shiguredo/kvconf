-module(kvconf_file).

-export([open/1]).

-spec open(binary()) -> {ok, map()} | {error, {atom(), any(), non_neg_integer()}}.
open(Path) ->
    case file:open(Path, [binary, raw]) of
        {error, Reason} ->
            {open_error, Reason, 0};
        {ok, File} ->
            parse_lines(File, #{}, 1)
    end.


parse_lines(File, Configurations, LineNumber) ->
    case file:read_line(File) of
        eof ->
            {ok, Configurations};
        {error, Reason} ->
            {error, {read_line_error, Reason, LineNumber}};
        {ok, [$# | _]} ->
            parse_lines(File, Configurations, LineNumber + 1);
        {ok, Line} ->
            case re:run(Line, <<" *([^ ]*) *= *([^ ]*) *">>, [{capture, all, binary}]) of
                nomatch ->
                    case re:run(Line, <<"^ *$">>) of
                        %% 空白だけの行はスキップする
                        {match, _} ->
                            parse_lines(File, Configurations, LineNumber + 1);
                        _ ->
                            {error, {invalid_format, Line, LineNumber}}
                    end;
                {match, [_, Key, Value]} ->
                    case maps:is_key(Key, Value) of
                        true ->
                            {error, {duplicated_key, Key, LineNumber}};
                        false ->
                            parse_lines(File, Configurations#{Key => {Value, LineNumber}}, LineNumber + 1)
                    end
            end
    end.

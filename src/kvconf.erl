-module(kvconf).

-export([open/2]).
-export([set_value/2, get_value/1]).

-export_type([key/0, type/0, is_required/0, definition/0]).

-type key() :: atom().

-type type() :: string | list_string | {integer, integer(), integer()} |
                ipv4_address | list_ipv4_address | ipv6_address | list_ipv6_address |
                host | port_number | boolean | http_uri | list_http_uri | list_to_binary.
-type is_required() :: required | optional.
-type definition() :: {key(), type(), is_required()} | {key(), type(), optional, term()}.

-spec open([definition()], binary()) -> ok | {error, {atom(), key(), any(), non_neg_integer()}}.
open(Definitions, Path) ->
    case kvconf_file:open(Path) of
        {ok, Configurations} ->
            kvconf_validate:validate(Configurations, Definitions);
        {error, Reason} ->
            {error, Reason}
    end.


set_value(Key, Value) ->
    ok = persistent_term:put(Key, Value).


get_value(Key) ->
    persistent_term:get(Key).

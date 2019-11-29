-module(kvconf).

-export([open/3]).

-export_type([key/0, type/0, is_required/0, definition/0]).

-type key() :: atom().

-type type() :: string | list_string | {integer, integer(), integer()} |
                ipv4_address | list_ipv4_address | ipv6_address | list_ipv6_address |
                host | port_number | boolean | http_uri | list_http_uri | list_to_binary.
-type is_required() :: required | optional.
-type definition() :: {key(), type(), is_required()} | {key(), type(), optional, term()}.

-spec open(atom(), [definition()], binary()) -> ok | {error, {atom(), any(), non_neg_integer()}}.
open(Application, Definitions, Path) ->
    case kvconf_file:open(Path) of
        {ok, Configurations} ->
            kvconf_validate:validate(Application, Configurations, Definitions);
        {error, Reason} ->
            {error, Reason}
    end.

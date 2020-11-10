-include_lib("eunit/include/eunit.hrl").

-record(kvc_atom, {
          candidates :: [atom() | {binary(), atom()}]
         }).

-record(kvc_string, {
         }).

-record(kvc_integer, {
          min :: integer(),
          max :: integer() | infinity
         }).

-record(kvc_float, {
          min :: float(),
          max :: float() | infinity
         }).

-record(kvc_boolean, {
         }).

-record(kvc_ipv4_address, {
         }).

-record(kvc_ipv6_address, {
         }).

-record(kvc_list_ipv4_address, {
         }).

-record(kvc_list_ipv6_address, {
         }).

-record(kvc_port_number, {
         }).

-record(kvc_http_uri, {
         }).


%% 入力を許可する値の単位
-type in_time_unit() :: ms | s | min | h.
-type out_time_unit() :: second | millisecond | microsecond.

-record(kvc_interval, {
          min :: {non_neg_integer(), in_time_unit()},
          max :: {non_neg_integer(), in_time_unit()} | infinity,
          out_time_unit :: out_time_unit()
         }).


-record(kvc, {
          key :: kvconf:key(),
          type :: kvconf:type(),
          required = false :: boolean(),
          default = undefined :: any()
         }).

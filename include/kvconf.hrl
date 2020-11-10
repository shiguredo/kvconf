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

-record(kvc_interval, {
          min :: non_neg_integer(),
          max :: non_neg_integer(),
          unit :: us | ms | s | min | h | d | y,
          default :: undefined | non_neg_integer()
         }).


-type key() :: atom().
-type type() :: #kvc_atom{} | #kvc_string{} | #kvc_integer{} | #kvc_float{} |
                #kvc_boolean{} | #kvc_ipv4_address{} | #kvc_ipv6_address{} |
                #kvc_list_ipv4_address{} | #kvc_list_ipv6_address{} |
                #kvc_port_number{} | #kvc_http_uri{} | #kvc_interval{}.

-record(kvc, {
          key :: key(),
          type :: type(),
          required = false :: boolean(),
          default = undefined :: any()
         }).

-include_lib("eunit/include/eunit.hrl").

-record(kvc_atom, {
          candidates :: [atom() | {binary(), atom()}]
         }).

-record(kvc_list_atom, {
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
          min :: {non_neg_integer(), kvconf:in_time_unit()},
          max :: {non_neg_integer(), kvconf:in_time_unit()} | infinity,
          out_time_unit :: kvconf:out_time_unit()
         }).


%% 中間証明書を含んだ証明書
-record(kvc_pkix_fullchain_pem_file, {
         }).

%% 証明書の秘密鍵
-record(kvc_pkix_privkey_pem_file, {
         }).


-record(kvc_pkix_cert_pem_file, {
         }).


%% TODO: dir を指定できる、未実装
%% openssl の -CAPath に相当
-record(kvc_pkix_cacert_path, {
         }).


-record(kvc, {
          key :: kvconf:key(),
          type :: kvconf:type(),
          required = false :: boolean(),
          default = undefined :: any()
         }).

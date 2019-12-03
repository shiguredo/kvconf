%% 内部実装
-type requirement() :: required | optional | {optional, term()}.
-type definition_internal() :: {kvconf:key(), kvconf:type(), kvconf:required()}
                             | {kvconf:key(), kvconf:type(), optional, term()}.

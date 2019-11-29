-module(kvconf_tests).

-include_lib("eunit/include/eunit.hrl").

smoke_test() ->
    ?debugVal(file:get_cwd()),
    ok = kvconf:open(kernel, [], <<"test/smoke_test.conf">>).

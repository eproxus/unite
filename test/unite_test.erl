-module(unite_test).

-include_lib("eunit/include/eunit.hrl").

%--- API -----------------------------------------------------------------------

empty_test() -> ok.

%--- Demo Tests ----------------------------------------------------------------

-ifdef(DEMO).

exit_test() -> exit(foo).

-endif.

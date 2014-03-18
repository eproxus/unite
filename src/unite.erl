-module(unite).

% API
-export([test/1]).

%--- API ----------------------------------------------------------------------

test(Modules) ->
    eunit:test(Modules).

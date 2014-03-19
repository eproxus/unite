-module(unite).

% API
-export([test/1]).

%--- API ----------------------------------------------------------------------

test(Specs) -> test(Specs, []).

test(Specs, Options) ->
    Format = proplists:get_value(format, Options, unite_compact),
    Tests = discover_tests(Specs),
    {Format, {Passed, Failed}} = run_tests(Tests, {Format, {0, 0}}),
    Format:test_summary(Passed, Failed, 0).

%--- Internal Functions -------------------------------------------------------

discover_tests(Specs) when is_list(Specs) ->
    [discover_tests(S) || S <- Specs];
discover_tests(Module) when is_atom(Module) ->
    [discover_tests({Module, F, A}) || {F, A} <- Module:module_info(exports)];
discover_tests({M, F, A}) ->
    case ends_with(F, "_test") of
        true  -> fun M:F/A;
        false ->
            case ends_with(F, "_test_") of
                true  -> discover_tests(M:F());
                false -> []
            end
    end;
discover_tests({Line, Fun}) when is_integer(Line), is_function(Fun) ->
    Fun.

ends_with(Atom, String) ->
    Binaries = [atom_to_binary(Atom, latin1), list_to_binary(String)],
    case binary:longest_common_suffix(Binaries) of
        L when L == length(String) -> true;
        _                          -> false
    end.

run_tests([{M, F, 0}|Tests], State) ->
    run_tests(Tests, run_test(fun M:F/0, State));
run_tests([Deep|Tests], State) when is_list(Deep) ->
    run_tests(Tests, run_tests(Deep, State));
run_tests([Fun|Tests], State) when is_function(Fun) ->
    run_tests(Tests, run_test(Fun, State));
run_tests([], State) ->
    State.

run_test(F, {Format, {Passed, Failed}}) ->
    ok = F(),
    Format:test_case(),
    {Format, {Passed + 1, Failed}}.

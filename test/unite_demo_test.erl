% NOTE: This is a demo test module, only for output purposes. Use like this:
%
%    $ rebar3 as demo eunit

-module(unite_demo_test).

-include_lib("eunit/include/eunit.hrl").

%--- Demo Tests ----------------------------------------------------------------

-ifdef(DEMO).

assert_fail_test() ->
    io:format("THIS IS ONLY SHOWN IN FAILURES~n"),
    ?assert(false).

assert_test() ->
    io:format("SHOULD NOT BE VISIBLE~n"),
    ?assert(true).

assert_equal_test() ->
    ?assertEqual(
        #{
            value => 1,
            list => [a, b, c, d, e],
            tuple => {foo, bar},
            binary => <<"test string">>,
            map => #{a => 1, b => 2, c => value}
        },
        #{
            value => 2,
            list => [a, b, x, d, e],
            tuple => {foo, baz},
            binary => <<"test binary">>,
            map => #{foo => baz, bar => qux}
        }
    ).

assert_match_test() ->
    ?assertMatch(
       X when X < 1,
       2
    ).

error_test() -> error(error_in_test).

exit_test() -> exit(exit_in_test).

throw_test() -> throw(throw_in_test).

long_test() -> timer:sleep(250).

short_test() -> timer:sleep(50).

% EUnit has a bug where not all of the below cases are catched. To see each
% individual case for sure, comment out the others:

setup_test_() ->
    {setup, fun() -> error(error_in_setup) end, fun(_) -> ok end, [?_assert(true)]}.

cleanup_test_() ->
    {setup, fun() -> ok end, fun(_) -> error(error_in_cleanup) end, [?_assert(true)]}.

instantiation_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun(_) -> error(error_in_instantiation) end}.

bad_spec_test_() -> {foo, bar, baz}.

single_module_not_found_test_() -> foo.

module_not_found_test_() -> {foo, bar}.

function_not_found_test_() -> {?MODULE, bar}.

generator_test_() ->
    error(error_in_generator).

-endif.

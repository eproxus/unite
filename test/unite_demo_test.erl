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
            binary => <<"test string ~p">>,
            map => #{a => 1, b => 2, c => value}
        },
        (fun() -> #{
            value => 2,
            list => [a, b, x, d, e],
            tuple => {foo, baz},
            binary => <<"test binary ~p">>,
            map => #{foo => baz, bar => qux}
        } end)()
    ).

assert_match_test() ->
    ?assertMatch(
       X when X < 1,
       (fun() -> 2 end)()
    ).

assert_throw_test() ->
    ?assertThrow(foo, throw(foo)),
    ?assertThrow(foo, throw(bar)).

assert_error_test() ->
    ?assertError(foo, error(foo)),
    ?assertError(foo, error(bar)).

assert_exit_test() ->
    ?assertExit(foo, exit(foo)),
    ?assertExit(foo, exit(bar)).

error_test() -> error(error_in_test).

exit_test() -> exit(exit_in_test).

throw_test() -> throw(throw_in_test).

long_test() -> timer:sleep(250).

short_test() -> timer:sleep(50).

bad_test_instantiator_test_() -> {foreach, fun() -> ok end, [fun(_) -> ok end]}.

io_format_test() ->
    % If a control code sneaks into the stack trace, we have to make sure we can
    % io:format it properly when Unite pretty prints:
    apply(io, format, ["~p", []]).

% Generators

setup_test_() ->
    {setup, fun() -> error(error_in_setup) end, fun(_) -> ok end, [?_assert(true)]}.

cleanup_test_() ->
    {setup, fun() -> ok end, fun(_) -> error(error_in_cleanup) end, [?_assert(true)]}.

instantiation_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, fun(_) -> error(error_in_instantiation) end}.

timeout_test_() ->
    {timeout, 0, [fun() -> timer:sleep(1000) end]}.

% EUnit has a bug where the below cases are not included if the above
% generators fail. To see these, comment out the generators above and every
% other test case below:

bad_spec_test_() -> {foo, bar, baz}.

single_module_not_found_test_() -> foo.

module_not_found_test_() -> {foo, bar}.

function_not_found_test_() -> {?MODULE, bar}.

generator_test_() ->
    error(error_in_generator).

-endif.

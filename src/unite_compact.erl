-module(unite_compact).

-behaviour(eunit_listener).

% EUnit Callbacks
-export([start/0]).
-export([start/1]).
-export([init/1]).
-export([handle_begin/3]).
-export([handle_end/3]).
-export([handle_cancel/3]).
-export([terminate/2]).

-export([ioindent/2]).

% Clear line: "\e[2K"

-record(s, {
    start = now(),
    failures = []
}).

%--- EUnit Callbacks ----------------------------------------------------------

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
    #s{}.

handle_begin(_Type, _Data, State) ->
    State.

handle_end(test, Data, State) ->
    case proplists:get_value(status, Data) of
        ok ->
            io:format(color:green(".")),
            State;
        skip ->
            io:format(color:yellow("S")),
            State;
        {error, _} ->
            io:format(color:redb("F")),
            State#s{failures = State#s.failures ++ [Data]}
    end;
handle_end(_Type, _Data, State) ->
    State.

handle_cancel(group, Data, State) ->
    case proplists:get_value(reason, Data) of
        undefined ->
            State;
        _Else ->
            io:format(color:yellow("C")),
            State#s{failures = State#s.failures ++ [Data]}
    end;
handle_cancel(_Type, _Data, State) ->
    State.

terminate({ok, Result}, State) ->
    print_failures(State#s.failures),
    print_summary(Result, State).

%--- Internal Functions -------------------------------------------------------

print_failures([]) -> ok;
print_failures(Failures) ->
    Indexed = lists:zip(lists:seq(1, length(Failures)), Failures),
    [print_failure(I, F) || {I, F} <- Indexed],
    io:format("~n").

% Individual Test Case

print_failure(Index, Failure) ->
    Reason = proplists:get_value(reason, Failure),
    Info = proplists:get_value(status, Failure, Reason),

    {Header, Details} = format_info(Failure, Info),
    io:format("~n~n ~p) ~s~n", [Index, Header]),
    io:format(ioindent(4, Details)),
    case format_output(Failure) of
        undefined -> ok;
        Output    -> io:format("~n~s", [ioindent(4, Output)])
    end.

format_info(Failure, {error, {error, {assertion_failed, Info}, ST}}) ->
    {
        color:red(format_case(Failure, ST)),
        [color:redb("Assert failed: "), proplists:get_value(expression, Info)]
    };
format_info(Failure, {error, {error, {assertEqual_failed, Info}, ST}}) ->
    Expected = proplists:get_value(expected, Info),
    Actual = proplists:get_value(value, Info),
    Exp = diff_prep_term(Expected),
    Act = diff_prep_term(Actual),
    Diff = tdiff:diff(Exp, Act),
    {
        color:red(format_case(Failure, ST)),
        io_lib:format("~s ~s~n~s", [
            color:redb("Assert equal failed:"),
            [
                color:blueb("[-Expected-]"),
                " ",
                color:yellowb("[+Actual+]")
            ],
            format_diff(Diff)
        ])
    };
format_info(Failure, {error, {E, R, ST}}) ->
    {
        color:red(format_case(Failure, ST)),
        [
            color:redb("Exception: "),
            io_lib:format("~n", []),
            color:red(format_exception(E, R, ST))
        ]
    };
format_info(Failure, {abort, {Reason, {E, R, ST}}}) ->
    {
        color:yellow(format_case(Failure, ST)),
        [
            color:yellowb(case Reason of
                setup_failed -> "Setup failed: ";
                cleanup_failed -> "Cleanup failed: "
            end),
            io_lib:format("~n", []),
            color:yellow(format_exception(E, R, ST))
        ]
    };
format_info(_Failure, {abort, {bad_test, Test}}) ->
    {
        color:yellow("Bad test specification:"),
        [
            color:yellow(io_lib:format("~p", [Test]))
        ]
    };
format_info(Failure, {abort, {generator_failed, {MFA, {E, R, ST}}}}) ->
    {
        color:yellow(format_case(Failure, [add_info(MFA, ST)])),
        [
            color:yellowb("Generator failed: "),
            io_lib:format("~n", []),
            color:yellow(format_exception(E, R, ST))
        ]
    }.

diff_prep_term(Term) ->
    {ok, Cols} = io:columns(),
    Pretty = io_lib_pretty:print(Term, 1, Cols, -1),
    Flat = iolist_to_binary(Pretty),
    TermSplit = "([,\\[\\]\\{\\}])",
    re:split(Flat, TermSplit, [trim]).

format_diff([]) ->
    [];
format_diff([{eq, Str}|Rest]) ->
    [Str|format_diff(Rest)];
format_diff([{del, Str}|Rest]) ->
    [color:blue(["[-", Str, "-]"])|format_diff(Rest)];
format_diff([{ins, Str}|Rest]) ->
    [color:yellow(["[+", Str, "+]"])|format_diff(Rest)].

format_case(Failure, ST) ->
    case proplists:get_value(desc, Failure) of
        undefined -> format_source(Failure, ST);
        Desc ->
            io_lib:format("~s~n~s", [
                Desc,
                ioindent(4, format_source(Failure, ST))
            ])
    end.

format_source(Failure, ST) ->
    case proplists:get_value(source, Failure) of
        undefined ->
            format_stack_line(hd(ST));
        MFA ->
            format_stack_line(add_info(MFA, ST))
    end.

format_stack_line({_M, F, A, I}) ->
    {File, L} = {proplists:get_value(file, I), proplists:get_value(line, I)},
    io_lib:format("~p/~p (~s:~p)", [F, A, File, L]).

format_exception(Error, Reason, Stacktrace) ->
    lib:format_exception(1, Error, Reason, Stacktrace,
        fun(_M, _F, _A) -> false end,
        fun(T, I) ->
            {ok, Cols} = io:columns(),
            io_lib_pretty:print(T, I, Cols, -1)
        end
    ).

format_output(Failure) ->
    case proplists:get_value(output, Failure) of
        <<>> -> undefined;
        undefined -> undefined;
        Output ->
            [
                color:blackb("Output:"),
                io_lib:format("~n", []),
                ioindent(2, Output)
            ]
    end.

% Summary

print_summary(Result, State) ->
    case get_all(Result, [pass, fail, skip, cancel]) of
        [0, 0, 0, 0] ->
            ok;
        [Pass, Fail, Skip, Cancel] ->
            Seconds = timer:now_diff(now(), State#s.start) / 1000000,
            Time = float_to_list(Seconds, [{decimals, 2}, compact]),
            io:format("~n~s~n", [iolist_to_binary(iojoin([
                non_zero(Pass, green, plural(Pass, "test", "passed")),
                non_zero(Fail, red, plural(Fail, "test", "failed")),
                non_zero(Skip, yellow, plural(Skip, "test", "skipped")),
                non_zero(Cancel, yellow, plural(Cancel, "fixture", "cancelled")),
                color:blackb(io_lib:format("(~s s)", [Time]))
            ], "  "))])
    end.

plural(Number, Noun, Postfix) ->
    Text = case Number of 1 -> Noun; Number -> [Noun, "s"] end,
    [i2b(Number), " ", Text, " ", Postfix].

% Utilities

get_all(Proplist, Keys) ->
    [proplists:get_value(K, Proplist) || K <- Keys].

i2b(Integer) -> integer_to_binary(Integer).

non_zero(Int, Colors, IOData) ->
    case Int of
        0 -> [];
        _ -> colorize(IOData, Colors)
    end.

colorize(String, []) ->
    String;
colorize(String, [Color|Colors]) ->
    colorize(color:Color(String), Colors);
colorize(String, Color) when is_atom(Color) ->
    colorize(String, [Color]).

iojoin([], _Separator)         -> [];
iojoin([[]|List], Separator)   -> iojoin(List, Separator);
iojoin([Item], _Separator)     -> Item;
iojoin([Item|List], Separator) -> [Item, Separator, iojoin(List, Separator)].

ioindent(Indent, IOData) when is_integer(Indent) ->
    Spacing = iolist_to_binary(lists:duplicate(Indent, 32)),
    [Spacing, ioindent(Spacing, IOData)];
ioindent(Spacing, [10|IOData]) ->
    [10, Spacing|ioindent(Spacing, IOData)];
ioindent(Spacing, Binary) when is_binary(Binary) ->
    binary:replace(Binary, <<"\n">>, <<"\n", Spacing/binary>>, [global]);
ioindent(Spacing, [Sub|IOData]) ->
    [ioindent(Spacing, Sub)|ioindent(Spacing, IOData)];
ioindent(_Spacing, []) ->
    [];
ioindent(_Spacing, Other) ->
    Other.

add_info(_MFA, [])                      -> [];
add_info({M, F, A}, [{M, _, _, I}|_ST]) -> {M, F, A, I};
add_info(MFA, [_Line|ST])               -> add_info(MFA, ST).

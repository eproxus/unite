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
    cases = [],
    profile = false,
    profile_max = 10
}).

%--- EUnit Callbacks ----------------------------------------------------------

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    case get(profile, Options) of
        undefined ->
            #s{};
        true ->
            #s{profile = true};
        Max when is_integer(Max), Max >= 0 ->
            #s{profile = true, profile_max = Max}
    end.

handle_begin(_Type, _Data, State) ->
    State.

handle_end(test, Data, State) ->
    case get(status, Data) of
        ok         -> io:format(color:green("."));
        skip       -> io:format(color:yellow("S"));
        {error, _} -> io:format(color:redb("F"))
    end,
    State#s{cases = State#s.cases ++ [Data]};
handle_end(_Type, _Data, State) ->
    State.

handle_cancel(group, Data, State) ->
    case get(reason, Data) of
        undefined ->
            State;
        _Else ->
            io:format(color:yellow("C")),
            State#s{cases = State#s.cases ++ [Data]}
    end;
handle_cancel(_Type, _Data, State) ->
    State.

terminate({ok, Result}, #s{cases = Cases} = State) ->
    print_failures(lists:filter(
        fun(C) ->
            case get(status, C) of
                {error, _} ->
                    true;
                _  ->
                    case get(reason, C) of
                        {abort, _} -> true;
                        _          -> false
                    end
            end
        end,
        Cases
    )),
    print_times(State),
    print_summary(Result, State).

%--- Internal Functions -------------------------------------------------------

print_failures([]) -> ok;
print_failures(Failures) ->
    Indexed = lists:zip(lists:seq(1, length(Failures)), Failures),
    [print_failure(I, F) || {I, F} <- Indexed],
    io:format("~n").

% Individual Test Case

print_failure(Index, Failure) ->
    Reason = get(reason, Failure),
    Info = get(status, Failure, Reason),

    {Header, Details} = format_info(Failure, Info),
    io:format("~n~n ~p) ~s~n", [Index, Header]),
    io:format(ioindent(4, Details)),
    case format_output(Failure) of
        undefined -> ok;
        Output    -> io:format("~n~s", [ioindent(4, Output)])
    end.

format_info(Failure, {error, {error, {assertion_failed, Info}, ST}}) ->
    Expr = get(expression, Info),
    {
        color:red(format_case(Failure, ST)),
        [color:redb("Assert failed: "), format_macro_string(Expr)]
    };
format_info(Failure, {error, {error, {assertEqual_failed, Info}, ST}}) ->
    Expected = get(expected, Info),
    Actual = get(value, Info),
    Exp = diff_prep_term(Expected),
    Act = diff_prep_term(Actual),
    Diff = tdiff:diff(Exp, Act),
    {
        color:red(format_case(Failure, ST)),
        io_lib:format("~s ~s~n~s", [
            color:redb("Assert equal failed!"),
            [
                color:blueb("-Expected-"),
                " ",
                color:yellowb("+Actual+")
            ],
            format_diff(Diff)
        ])
    };
format_info(Failure, {error, {error, {assertMatch_failed, Info}, ST}}) ->
    Expr = get(expression, Info),
    Pattern = get(pattern, Info),
    Value = get(value, Info),
    {
        color:red(format_case(Failure, ST)),
        io_lib:format("~s~n~s~n~s~n~s~n~s~n~s~n~s~n", [
            color:redb("Assert match failed!"),
            color:magentab("Expression:"),
            ioindent(4, format_macro_string(Expr)),
            color:blueb("Pattern:"),
            ioindent(4, format_macro_string(Pattern)),
            color:yellowb("Actual:"),
            ioindent(4, format_term(Value, 0, 8))
        ])
    };
format_info(Failure, {error, {error, {assertException_failed, Info}, ST}}) ->
    case get(unexpected_exception, Info) of
        undefined ->
            Success = get(unexpected_success, Info),
            Term = format_term(Success, 22, 4),
            {
                color:red(format_case(Failure, ST)),
                case multiline(Term) of
                    true ->
                        io_lib:format("~s~n~s", [
                            color:redb("Unexpected success!"),
                            color:red(format_term(Success, 0, 4))
                        ]);
                    false ->
                        [
                            color:redb("Unexpected success:"),
                            " ",
                            color:red(Term)
                        ]
                end
            };
        {E, R, NewST} ->
            {
                color:red(format_case(Failure, ST)),
                io_lib:format("~s~n~s", [
                    color:redb("Unexpected exception:"),
                    color:red(format_exception(E, R, NewST))
                ])
            }
    end;
format_info(Failure, {error, {E, R, ST}}) ->
    {
        format_case(Failure, ST, red),
        [
            color:redb("Uncaught exception! "),
            io_lib:format("~n", []),
            color:red(format_exception(E, R, ST))
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
            color:yellowb("Generator failed!"),
            io_lib:format("~n", []),
            color:yellow(format_exception(E, R, ST))
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
    }.

diff_prep_term(Term) ->
    Pretty = format_term(Term, 0, 0),
    Flat = iolist_to_binary(Pretty),
    TermSplit = "([,\\[\\]\\{\\}]|\\s+=>\\s+)",
    re:split(Flat, TermSplit, [trim]).

format_term(Term, Indent, Outer) ->
    io_lib_pretty:print(Term, Indent, columns() - Outer, -1).

format_diff([]) ->
    [];
format_diff([{eq, Str}|Rest]) ->
    [Str|format_diff(Rest)];
format_diff([{del, Str}|Rest]) ->
    [color:blue(["-", Str, "-"])|format_diff(Rest)];
format_diff([{ins, Str}|Rest]) ->
    [color:yellow(["+", Str, "+"])|format_diff(Rest)].


format_case(Failure, ST) -> format_case(Failure, ST, white).

format_case(Failure, ST, Color) ->
    case get(desc, Failure) of
        undefined -> format_source(Failure, ST);
        Desc ->
            io_lib:format("~s~n~s", [
                colorize([$", Desc, $"], cyan),
                colorize(ioindent(4, format_source(Failure, ST)), Color)
            ])
    end.

format_source(Failure, ST) ->
    case get(source, Failure) of
        undefined ->
            format_stack_line(hd(ST));
        MFA ->
            format_stack_line(add_info(MFA, ST))
    end.

format_stack_line({M, F, A, I}) ->
    case {get(file, I), get(line, I)} of
        {undefined, undefined} ->
            io_lib:format("~p:~p/~p", [M, F, A]);
        {File, L} ->
            io_lib:format("~p/~p (~s:~p)", [F, A, File, L])
    end.

format_exception(Error, Reason, Stacktrace) ->
    lib:format_exception(1, Error, Reason, Stacktrace,
        fun(_M, _F, _A) -> false end,
        fun(T, I) ->
            io_lib_pretty:print(T, I, columns(), -1)
        end
    ).

format_output(Failure) ->
    case get(output, Failure) of
        <<>> -> undefined;
        undefined -> undefined;
        Output ->
            [
                color:blackb("Output:"),
                io_lib:format("~n", []),
                ioindent(2, Output)
            ]
    end.

fixup_record_accessors([{'#', _} = Bang, {atom, _, _} = Record, {dot, Line}, {atom, _, _} = Member | Rest]) ->
    [Bang, Record, {'.', Line}, Member | fixup_record_accessors(Rest)];
fixup_record_accessors([X | Rest]) ->
    [X | fixup_record_accessors(Rest)];
fixup_record_accessors([]) ->
    [].

format_macro_string(Str) ->
    case lists:member($?, Str) of
        true ->
            [C || C <- Str, C =/= $ ];
        false ->
            {ok, S, _} = erl_scan:string(Str ++ "."),
            %% Eunit expression contains record accessors as 'AA # record . field' which can't be parsed.
            S2 = fixup_record_accessors(S),
            {ok, P} = erl_parse:parse_exprs(S2),
            erl_pp:exprs(P)
    end.

% Profiling

print_times(#s{profile_max = Max, cases = Cases, profile = P}) when P ->
    Times = [{get(time, C), format_case(C, [])} || C <- Cases],
    Top = lists:sublist(lists:reverse(lists:sort(Times)), Max),
    case length(Top) of
        0 ->
            ok;
        N ->
            Title = colorize(io_lib:format("Top ~p slowest tests:", [N]), yellow),
            io:format("~n~n  ~s~n", [Title]),
            [print_time(T, C) || {T, C} <- Top]
    end;
print_times(_State) ->
    ok.

print_time(Ms, Case) ->
    Time = colorize(string:left(format_time(Ms), 10), red),
    io:format("    ~s~n      ~s~n", [Case, Time]).

% Summary

print_summary(Result, State) ->
    case get_all(Result, [pass, fail, skip, cancel]) of
        [0, 0, 0, 0] ->
            ok;
        [Pass, Fail, Skip, Cancel] ->
            Ms = timer:now_diff(now(), State#s.start) / 1000,
            Time = format_time(Ms),
            io:format("~n~s~n", [iolist_to_binary(iojoin([
                non_zero(Pass, green, plural(Pass, "test", "passed")),
                non_zero(Fail, red, plural(Fail, "test", "failed")),
                non_zero(Skip, yellow, plural(Skip, "test", "skipped")),
                non_zero(Cancel, yellow, plural(Cancel, "fixture", "cancelled")),
                color:blackb(io_lib:format("(~s)", [Time]))
            ], "  "))])
    end.

plural(Number, Noun, Postfix) ->
    Text = case Number of 1 -> Noun; Number -> [Noun, "s"] end,
    [i2b(Number), " ", Text, " ", Postfix].

% Utilities

format_time(Ms) -> io_lib:format("~.2f s", [Ms / 1000]).

get_all(Proplist, Keys) ->
    [get(K, Proplist) || K <- Keys].

i2b(Integer) -> list_to_binary(integer_to_list(Integer)).

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

add_info({M, F, A}, [])                 -> {M, F, A, []};
add_info({M, F, A}, [{M, _, _, I}|_ST]) -> {M, F, A, I};
add_info(MFA, [_Line|ST])               -> add_info(MFA, ST).

multiline([10|_IOData]) ->
    true;
multiline([List|IOData]) when is_list(List) ->
    multiline(List) orelse multiline(IOData);
multiline([_|IOData]) ->
    multiline(IOData);
multiline(IOData) when is_binary(IOData) ->
    binary:match(IOData, <<"\n">>) =/= nomatch;
multiline([]) ->
    false.

get(Key, Proplist)          -> proplists:get_value(Key, Proplist).
get(Key, Proplist, Default) -> proplists:get_value(Key, Proplist, Default).

columns() -> case io:columns() of {ok, Columns} -> Columns; _Error -> 80 end.

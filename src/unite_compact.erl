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
    io:format(color:yellow("C")),
    State#s{failures = State#s.failures ++ [Data]};
handle_cancel(_Type, _Data, State) ->
    State.

terminate({ok, Result}, State) ->
    print_failures(State#s.failures),
    print_summary(Result, State).

%--- Internal Functions -------------------------------------------------------

print_failures([]) -> ok;
print_failures(Failures) ->
    io:format("~n"),
    io:format("~n"),
    [print_failure(F) || F <- Failures].

print_failure(Failure) ->
    io:format("~p~n~n", [Failure]),
    {Type, Info, Case} = failure_info(Failure),
    io:format("~s~n", [
        color:redb([format_type(Type),
        " in ",
        format_case(Case)])
    ]),
    io:format("~s~n", [ioindent(4, Info)]),
    case proplists:get_value(output, Failure) of
        <<>> -> ok;
        undefined -> ok;
        Output ->
            io:format("~s~n", [
                ioindent(4, [
                    color:blackb("Output:"),
                    io_lib:format("~n", []),
                    ioindent(4, Output)
                ])
            ])
    end.

failure_info(Failure) ->
    % io:format("~p~n", [Failure]),
    case proplists:get_value(status, Failure, proplists:get_value(reason, Failure)) of
        {error, {error, Assert = {assertEqual_failed, _}, ST}} ->
            {fail, format_assert(Assert), hd(ST)};
        {error, {E, R, ST}} ->
            {M, F, A} = proplists:get_value(source, Failure),
            {fail, format_exception(E, R, ST), {M, F, A, []}};
        {abort, {setup_failed, {E, R, ST}}} ->
            {cancel, format_exception(E, R, ST), hd(ST)}
    end.

format_assert({assertEqual_failed, Info}) ->
    Expected = proplists:get_value(expected, Info),
    Actual = proplists:get_value(value, Info),
    io_lib:format("~s~n~s~n~s~n~s~n~s", [
        color:magenta(proplists:get_value(expression, Info)),
        color:blueb("Expected:"),
        ioindent(4, io_lib_pretty:print(Expected)),
        color:yellowb("Actual:"),
        ioindent(4, io_lib_pretty:print(Actual))
    ]).

format_exception(Error, Reason, Stacktrace) ->
    color:red(lib:format_exception(4, Error, Reason, Stacktrace,
        fun(_M, _F, _A) -> false end,
        fun(T, I) ->
            {ok, Cols} = io:columns(),
            io_lib_pretty:print(T, I, Cols - 4, -1)
        end
    )).

format_type(fail) -> "Failure";
format_type(cancel) -> "Cancel".

format_case({M, F, A, Info}) ->
    Function = io_lib:format("~p:~p/~p", [M, F, A]),
    case Info of
        [] ->
            Function;
        Info ->
            [
                Function,
                " (", proplists:get_value(file, Info),
                ", line ", integer_to_list(proplists:get_value(line, Info)),
                ")"
            ]
    end.

print_summary(Result, State) ->
    case get_all(Result, [pass, fail, skip, cancel]) of
        [0, 0, 0, 0] ->
            ok;
        [Pass, Fail, Skip, Cancel] ->
            Seconds = timer:now_diff(now(), State#s.start) / 1000000,
            Time = float_to_list(Seconds, [{decimals, 2}, compact]),
            io:format("~n~s~n", [iolist_to_binary(iojoin([
                non_zero(Pass, green, plural(Pass, "passed")),
                non_zero(Fail, red, plural(Fail, "failed")),
                non_zero(Skip, yellow, plural(Skip, "skipped")),
                non_zero(Cancel, yellow, plural(Cancel, "cancelled")),
                color:blackb(io_lib:format("(~s s)", [Time]))
            ], "  "))])
    end.

plural(Number, Postfix) ->
    Text = case Number of 1 -> "test"; Number -> "tests" end,
    [i2b(Number), " ", Text, " ", Postfix].

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

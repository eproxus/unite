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

% Clear line: "\e[2K"

-record(s, {
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
    print_summary(Result).

%--- Internal Functions -------------------------------------------------------

print_failures([]) -> ok;
print_failures(Failures) ->
    io:format("~n"),
    io:format("~n"),
    [print_failure(F) || F <- Failures].

print_failure(Failure) ->
    % io:format("~p~n~n", [Failure]),
    {Type, Error, Reason, Stacktrace, {M, F, A}} = failure_type(Failure),
    Ex = lib:format_exception(4, Error, Reason, Stacktrace,
        fun(_M, _F, _A) -> false end,
        fun(T, I) ->
            {ok, Cols} = io:columns(),
            io_lib_pretty:print(T, I, Cols - 4, -1)
        end
    ),
    Case = io_lib:format("~p:~p/~p", [M, F, A]),
    io:format("~s ~s~n", [format_type(Type), Case]),
    io:format("~s~n", [color:red(ioindent(Ex, 4))]).

failure_type(Failure) ->
    case proplists:get_value(status, Failure, proplists:get_value(reason, Failure)) of
        {error, {E, R, ST}} ->
            {fail, E, R, ST, proplists:get_value(source, Failure)};
        {abort, {setup_failed, {E, R, ST}}} ->
            {M, F, A, _} = hd(ST),
            {cancel, E, R, ST, {M, F, A}}
    end.

format_type(fail) -> color:redb("Failure:");
format_type(cancel) -> color:yellowb("Cancelled:").

print_summary(Result) ->
    case get_all(Result, [pass, fail, skip, cancel]) of
        [0, 0, 0, 0] ->
            ok;
        [Pass, Fail, Skip, Cancel] ->
            io:format("~n~s~n", [iolist_to_binary(iojoin([
                non_zero(Pass, green, [i2b(Pass), " tests passed"]),
                non_zero(Fail, red, [i2b(Fail), " tests failed"]),
                non_zero(Skip, yellow, [i2b(Skip), " tests skipped"]),
                non_zero(Cancel, yellow, [i2b(Cancel), " tests cancelled"])
            ], "  "))])
    end.

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

ioindent(IOData, Indent) when is_integer(Indent) ->
    Spacing = lists:duplicate(Indent, 32),
    [Spacing, ioindent(IOData, Spacing)];
ioindent([Sub|IOData], Indent) when is_list(Sub) ->
    [ioindent(Sub, Indent)|ioindent(IOData, Indent)];
ioindent([10|IOData], Indent) ->
    [10, Indent|ioindent(IOData, Indent)];
ioindent([], _Indent) ->
    [];
ioindent([Other|IOData], Indent) ->
    [Other|ioindent(IOData, Indent)].


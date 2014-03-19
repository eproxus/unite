-module(unite_verbose).

% API
-export([test_summary/3]).
-export([test_case/3]).

% Clear line: "\e[2K"

%--- API ----------------------------------------------------------------------

test_summary(Passed, Failed, Skipped) ->
    io:format("~n~s~n", [iolist_to_binary(iojoin([
        non_zero(Passed, green, [i2b(Passed), " tests passed"]),
        non_zero(Failed, red, [i2b(Failed), " tests failed"]),
        non_zero(Skipped, yellow, [i2b(Skipped), " tests skipped"])
    ], "  "))]).

test_case({Result, _}, Module, Function) ->
    io:format("~s ~p:~p~n", [result(Result), Module, Function]).

%--- Internal Functions -------------------------------------------------------

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

result(passed) -> color:green(string:right("Passed", 7));
result(failed) -> color:red(string:right("Failed", 7));
result(skipped) -> color:yellow(string:right("Skipped", 7)).

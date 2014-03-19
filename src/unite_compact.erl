-module(unite_compact).

% API
-export([test_summary/3]).
-export([test_case/0]).

% Clear line: "\e[2K"

%--- API ----------------------------------------------------------------------

test_summary(Passed, Failed, Skipped) ->
    io:format("~n~s~n", [iolist_to_binary(iojoin([
        non_zero(Passed, green, [i2b(Passed), " tests passed"]),
        non_zero(Failed, red, [i2b(Failed), " tests failed"]),
        non_zero(Skipped, yellow, [i2b(Skipped), " tests skipped"])
    ], "  "))]).

        % non_zero(Passed, [black, on_green]),
        % non_zero(Failed, [black, on_red]),
        % non_zero(Skipped, [black, on_yellow])

test_case() ->
    io:format(color:green(".")).

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

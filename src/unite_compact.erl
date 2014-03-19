-module(unite_compact).

% API
-export([test_summary/3]).
-export([test_case/0]).

% Clear line: "\e[2K"

%--- API ----------------------------------------------------------------------

test_summary(Passed, Failed, Skipped) ->
    {ok, Columns} = io:columns(),
    io:format("~n~s~n", [iolist_to_binary([
        non_zero(Passed, [black, on_green]),
        non_zero(Failed, [black, on_red]),
        non_zero(Skipped, [black, on_yellow])
    ])]).

test_case() ->
    io:format(color:green(".")).

%--- Internal Functions -------------------------------------------------------

non_zero(Int, Colors) ->
    IntS = [" ", integer_to_list(Int), " "],
    case Int of
        0 -> [];
        _ -> colorize(IntS, Colors)
    end.

colorize(String, []) ->
    String;
colorize(String, [Color|Colors]) ->
    colorize(color:Color(String), Colors).

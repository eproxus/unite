-module(unite_rebar).

% API
-export([unite/2]).

-define(UNITE_DIR, ".unite").

%--- API ----------------------------------------------------------------------

unite(Config, _AppFile) ->
    ok = filelib:ensure_dir(filename:join(unite_dir(), "dummy")),

    CodePath = code:get_path(),
    true = code:add_patha(unite_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),

    rebar_erlc_compiler:test_compile(Config, "unite", ?UNITE_DIR),
    Beams = rebar_utils:beams(?UNITE_DIR),
    Modules = [rebar_utils:beam_to_mod(?UNITE_DIR, B) || B <- Beams],
    unite:test(Modules),

    true = code:set_path(CodePath),

    ok.

%--- Internal Functions -------------------------------------------------------

unite_dir() ->
    filename:join(rebar_utils:get_cwd(), ?UNITE_DIR).

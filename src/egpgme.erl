-module(egpgme).
-on_load(init/0).

-export([
    context/0
]).

-define(NIF_NAME, "egpgme_nif").
-define(NOT_LOADED, error(nif_not_loaded)).

init() ->
    case code:priv_dir(egpgme) of
        {error, bad_name} ->
            SoName = filename:join("../priv", ?NIF_NAME);

        Dir ->
            SoName = filename:join(Dir, ?NIF_NAME)
    end,
    ok = erlang:load_nif(SoName, 0).

context() ->
    ?NOT_LOADED.

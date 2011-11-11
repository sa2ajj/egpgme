-module(egpgme).
-on_load(init/0).

-include("egpgme.hrl").

% error related functions
-export([
    strerror/1
]).

% context related functions
-export([
    context/0
]).

-define(NIF_NAME, "egpgme_nif").
-define(NOT_LOADED, error(nif_not_loaded)).

init() ->
    SoName = case code:priv_dir(egpgme) of
        {error, bad_name} ->
            filename:join("../priv", ?NIF_NAME);

        Dir ->
            filename:join(Dir, ?NIF_NAME)
    end,
    ok = erlang:load_nif(SoName, 0).

strerror(_) ->
    ?NOT_LOADED.

context() ->
    ?NOT_LOADED.

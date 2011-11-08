-module(egpgme).
-on_load(init/0).

-export([
    new/0,
    myfunction/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

new() ->
    ?NOT_LOADED.

myfunction(_Ref) ->
    ?NOT_LOADED.

% {{{ EUnit tests
-ifdef(TEST).
basic_test() ->
    {ok, Ref} = new(),
    ok = myfunction(Ref).
-endif.
% }}}

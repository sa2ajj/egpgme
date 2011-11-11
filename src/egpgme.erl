-module(egpgme).
-on_load(init/0).

-include("egpgme.hrl").

% helpers
-export([
    strerror/1,
    protocol_name/1,
    algo_name/1,
    hash_algo_name/1
]).

% context related functions
-export([
    context/0,
    protocol/1,
    set_protocol/2,
    armor/1,
    set_armor/2,
    textmode/1,
    set_textmode/2,
    include_certs/1,
    set_include_certs/2,
    keylist_mode/1,
    set_keylist_mode/2,
    engine_info/1,
    set_engine_info/4,
    signers_clear/1,
    signers_add/2,
    signers_enum/2,
    sig_notation_clear/1,
    sig_notation_add/4,
    sig_notation/1
]).

% data related functions
-export([
    data/0,
    data/1,
    read/2,
    write/2,
    seek/3,
    encoding/1,
    set_encoding/2
]).

% key related functions
-export([
    key/3
]).

% operations
-export([
    encrypt/5,
    encrypt_sign/5,
    decrypt/3,
    decrypt_verify/3,
    sign/4,
    verify/4,
    import/2,
    import_keys/2,
    export/4,
    export_keys/4,
    genkey/4,
    delete/3,
    edit/5,     % ??
    keylist_first/2,
    keylist_next/1,
    passwd/3,
    trustlist_first/3,
    trustlist_next/1
]).

% miscellaneous
-export([
    engine_info/0,
    set_engine_info/3,
    engine_check_version/1
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

% {{{ Helpers
strerror(_) ->
    ?NOT_LOADED.

protocol_name(_) ->
    ?NOT_LOADED.

algo_name(_) ->
    ?NOT_LOADED.

hash_algo_name(_) ->
    ?NOT_LOADED.
% }}}

% {{{ Context related functions
context() ->
    ?NOT_LOADED.

protocol(_) ->
    ?NOT_LOADED.

set_protocol(_, _) ->
    ?NOT_LOADED.

armor(_) ->
    ?NOT_LOADED.

set_armor(_, _) ->
    ?NOT_LOADED.

textmode(_) ->
    ?NOT_LOADED.

set_textmode(_, _) ->
    ?NOT_LOADED.

include_certs(_) ->
    ?NOT_LOADED.

set_include_certs(_, _) ->
    ?NOT_LOADED.

keylist_mode(_) ->
    ?NOT_LOADED.

set_keylist_mode(_, _) ->
    ?NOT_LOADED.

engine_info(_) ->
    ?NOT_LOADED.

set_engine_info(_, _, _, _) ->
    ?NOT_LOADED.

signers_add(_, _) ->
    ?NOT_LOADED.

signers_clear(_) ->
    ?NOT_LOADED.

signers_enum(_, _) ->
    ?NOT_LOADED.

sig_notation_clear(_) ->
    ?NOT_LOADED.

sig_notation_add(_, _, _, _) ->
    ?NOT_LOADED.

sig_notation(_) ->
    ?NOT_LOADED.
% }}}

% {{{ Data related functions
data() ->
    ?NOT_LOADED.

data(_) ->
    ?NOT_LOADED.

read(_, _) ->
    ?NOT_LOADED.

write(_, _) ->
    ?NOT_LOADED.

seek(_, _, _) ->
    ?NOT_LOADED.

encoding(_) ->
    ?NOT_LOADED.

set_encoding(_, _) ->
    ?NOT_LOADED.
% }}}

%% {{{ Key related functions
key(_, _, _) ->
    ?NOT_LOADED.
%% }}}

% {{{ Operations
encrypt(_, _, _, _, _) ->
    ?NOT_LOADED.

encrypt_sign(_, _, _, _, _) ->
    ?NOT_LOADED.

decrypt(_, _, _) ->
    ?NOT_LOADED.

decrypt_verify(_, _, _) ->
    ?NOT_LOADED.

sign(_, _, _, _) ->
    ?NOT_LOADED.

verify(_, _, _, _) ->
    ?NOT_LOADED.

import(_, _) ->
    ?NOT_LOADED.

import_keys(_, _) ->
    ?NOT_LOADED.

export(_, _, _, _) ->
    ?NOT_LOADED.

export_keys(_, _, _, _) ->
    ?NOT_LOADED.

genkey(_, _, _, _) ->
    ?NOT_LOADED.

delete(_, _, _) ->
    ?NOT_LOADED.

edit(_, _, _, _, _) ->
    ?NOT_LOADED.

keylist_first(_, _) ->
    ?NOT_LOADED.

keylist_next(_) ->
    ?NOT_LOADED.

passwd(_, _, _) ->
    ?NOT_LOADED.

trustlist_first(_, _, _) ->
    ?NOT_LOADED.

trustlist_next(_) ->
    ?NOT_LOADED.
% }}}

% {{{ Miscellaneous
engine_info() ->
    ?NOT_LOADED.

set_engine_info(_, _, _) ->
    ?NOT_LOADED.

engine_check_version(_) ->
    ?NOT_LOADED.
% }}}

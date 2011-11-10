-module(gpg_SUITE).

-export([
    all/0
]).

-export([
    t_encrypt/1,
    t_encrypt_sym/1,
    t_encrypt_sign/1,
    t_sign/1,
    t_signers/1,
    t_decrypt/1,
    t_verify/1,
    t_decrypt_verify/1,
    t_sig_notation/1,
    t_export/1,
    t_import/1,
    t_trustlist/1,
    t_edit/1,
    t_keylist/1,
    t_keylist_sig/1,
    t_wait/1,
    t_encrypt_large/1,
    t_file_name/1,
    t_gpgconf/1
]).

all() -> [
    t_encrypt,
    t_encrypt_sym,
    t_encrypt_sign,
    t_sign,
    t_signers,
    t_decrypt,
    t_verify,
    t_decrypt_verify,
    t_sig_notation,
    t_export,
    t_import,
    t_trustlist,
    t_edit,
    t_keylist,
    t_keylist_sig,
    t_wait,
    t_encrypt_large,
    t_file_name,
    t_gpgconf
].

t_encrypt(_Config) ->
    ok.

t_encrypt_sym(_Config) ->
    ok.

t_encrypt_sign(_Config) ->
    ok.

t_sign(_Config) ->
    ok.

t_signers(_Config) ->
    ok.

t_decrypt(_Config) ->
    ok.

t_verify(_Config) ->
    ok.

t_decrypt_verify(_Config) ->
    ok.

t_sig_notation(_Config) ->
    ok.

t_export(_Config) ->
    ok.

t_import(_Config) ->
    ok.

t_trustlist(_Config) ->
    ok.

t_edit(_Config) ->
    ok.

t_keylist(_Config) ->
    ok.

t_keylist_sig(_Config) ->
    ok.

t_wait(_Config) ->
    ok.

t_encrypt_large(_Config) ->
    ok.

t_file_name(_Config) ->
    ok.

t_gpgconf(_Config) ->
    ok.

#ifndef __EGPGME_INCLUDED__
#define __EGPGME_INCLUDED__

#include <locale.h>
#include <gpgme.h>

#include <erl_nif.h>

enum {
    EGPGME_CONTEXT = 0,
    EGPGME_DATA,
    EGPGME_KEY,
    EGPGME_LAST
};

    ERL_NIF_TERM egpgme_error(ErlNifEnv *, ERL_NIF_TERM);
    ERL_NIF_TERM egpgme_gpgme_error(ErlNifEnv *, gpgme_error_t);
    ERL_NIF_TERM egpgme_ok(ErlNifEnv *, ERL_NIF_TERM);

    ERL_NIF_TERM egpgme_strerror(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_algo_name(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_hash_algo_name(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_protocol_name(ErlNifEnv *, int, const ERL_NIF_TERM []);

    ERL_NIF_TERM egpgme_context_new(ErlNifEnv *, int, const ERL_NIF_TERM []);
    void egpgme_context_delete(ErlNifEnv *, void *);
    ERL_NIF_TERM egpgme_context_protocol(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_protocol(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_armor(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_armor(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_textmode(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_textmode(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_include_certs(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_include_certs(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_keylist_mode(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_keylist_mode(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_engine_info(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_set_engine_info(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_signers_clear(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_signers_add(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_signers_enum(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_sig_notation_clear(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_sig_notation_add(ErlNifEnv *, int, const ERL_NIF_TERM []);
    ERL_NIF_TERM egpgme_context_sig_notation(ErlNifEnv *, int, const ERL_NIF_TERM []);

    ERL_NIF_TERM egpgme_data_new(ErlNifEnv *, int, const ERL_NIF_TERM []);
    void egpgme_data_delete(ErlNifEnv *, void *);
#endif

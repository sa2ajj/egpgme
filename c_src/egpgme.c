#include "egpgme.h"

#define EGPGME_MODULE_STR  "egpgme"

typedef struct {
    gpgme_key_t key;
} egpgme_key;

static void egpgme_key_delete(ErlNifEnv *env, void *arg) {
    gpgme_key_release(((egpgme_key *)arg)->key);
}

ERL_NIF_TERM egpgme_error(ErlNifEnv *env, ERL_NIF_TERM err) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), err);
}

ERL_NIF_TERM egpgme_gpgme_error(ErlNifEnv *env, gpgme_error_t err) {
    ERL_NIF_TERM source = enif_make_int(env, gpgme_err_source(err));
    ERL_NIF_TERM code = enif_make_int(env, gpgme_err_code(err));

    return egpgme_error(env, enif_make_tuple2(env, source, code));
}

ERL_NIF_TERM egpgme_ok(ErlNifEnv *env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM _egpgme_strerror(ErlNifEnv *env, gpgme_error_t err) {
    char *message = enif_alloc(512);
    ERL_NIF_TERM result;

    // No check for return value as gpgme_strerror_r truncates the output
    gpgme_strerror_r(err, message, 512);

    result = enif_make_tuple2(env,
                              enif_make_string(env, gpgme_strsource(err), ERL_NIF_LATIN1),
                              enif_make_string(env, message, ERL_NIF_LATIN1));
    enif_free(message);

    return result;
}

ERL_NIF_TERM egpgme_strerror(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int size;
    const ERL_NIF_TERM *tuple;
    int source, code;

    // Check if we got a tuple of the right size
    if (!enif_get_tuple(env, argv[0], &size, &tuple) || size != 2) {
        return enif_make_badarg(env);
    }

    // Check if we got ints
    if (!enif_get_int(env, tuple[0], &source) ||
        !enif_get_int(env, tuple[1], &code)) {
        return enif_make_badarg(env);
    }

    return _egpgme_strerror(env, gpgme_err_make(source, code));
}

ERL_NIF_TERM egpgme_algo_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int algo;

    if (!enif_get_int(env, argv[0], &algo)) {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, gpgme_pubkey_algo_name(algo), ERL_NIF_LATIN1);
}

ERL_NIF_TERM egpgme_hash_algo_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int algo;

    if (!enif_get_int(env, argv[0], &algo)) {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, gpgme_hash_algo_name(algo), ERL_NIF_LATIN1);
}

ERL_NIF_TERM egpgme_protocol_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int protocol;

    if (!enif_get_int(env, argv[0], &protocol)) {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, gpgme_get_protocol_name(protocol), ERL_NIF_LATIN1);
}

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    ErlNifResourceType **egpgme_resources = NULL;

    if ((egpgme_resources = calloc(EGPGME_LAST, sizeof(ErlNifResourceType *))) == NULL) {
        return 1;
    }

    gpgme_check_version(NULL);
    // NOTE: enif does not support encodings other than latin-1,
    // so the code below needs to be rethought
    gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_CTYPE, NULL));
#ifdef LC_MESSAGES
    gpgme_set_locale(NULL, LC_MESSAGES, setlocale(LC_MESSAGES, NULL));
#endif
    egpgme_resources[EGPGME_CONTEXT] = \
        enif_open_resource_type(env, EGPGME_MODULE_STR, "context",
                                egpgme_context_delete,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    egpgme_resources[EGPGME_DATA] = \
        enif_open_resource_type(env, EGPGME_MODULE_STR, "data",
                                egpgme_data_delete,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    egpgme_resources[EGPGME_KEY] = \
        enif_open_resource_type(env, EGPGME_MODULE_STR, "key",
                                egpgme_key_delete,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    *priv_data = egpgme_resources;

    return 0;
}

static ErlNifFunc egpgme_funcs[] = {
    {"strerror", 1, egpgme_strerror},
    {"protocol_name", 1, egpgme_protocol_name},
    {"algo_name", 1, egpgme_algo_name},
    {"hash_algo_name", 1, egpgme_hash_algo_name},
    {"context", 0, egpgme_context_new},
    {"protocol", 1, egpgme_context_protocol},
    {"set_protocol", 2, egpgme_context_set_protocol},
    {"armor", 1, egpgme_context_armor},
    {"set_armor", 2, egpgme_context_set_armor},
    {"textmode", 1, egpgme_context_textmode},
    {"set_textmode", 2, egpgme_context_set_textmode},
    {"include_certs", 1, egpgme_context_include_certs},
    {"set_include_certs", 2, egpgme_context_set_include_certs},
    {"keylist_mode", 1, egpgme_context_keylist_mode},
    {"set_keylist_mode", 2, egpgme_context_set_keylist_mode},
#if 0
    {"engine_info", 1, egpgme_context_engine_info},
    {"set_engine_info", 4, egpgme_context_set_engine_info},
    {"signers_clear", 1, egpgme_context_signers_clear},
    {"signers_add", 2, egpgme_context_signers_add},
    {"signers_enum", 2, egpgme_context_signers_enum},
    {"sig_notation_clear", 1, egpgme_context_sig_notation_clear},
    {"sig_notation_add", 4, egpgme_context_sig_notation_add},
    {"sig_notation", 1, egpgme_context_sig_notation},
#endif
    {"data", 0, egpgme_data_new},
    {"data", 1, egpgme_data_new},
    {"read", 2, egpgme_data_read},
    {"write", 2, egpgme_data_write},
    {"seek", 3, egpgme_data_seek},
    {"encoding", 1, egpgme_data_encoding},
    {"set_encoding", 2, egpgme_data_set_encoding}
};

ERL_NIF_INIT(egpgme, egpgme_funcs, &on_load, NULL, NULL, NULL)

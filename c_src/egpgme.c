#include <locale.h>
#include <gpgme.h>

#include <erl_nif.h>

#define EGPGME_MODULE_STR  "egpgme"

enum {
    EGPGME_CONTEXT = 0,
    EGPGME_DATA,
    EGPGME_KEY,
    EGPGME_LAST
};

typedef struct {
    gpgme_ctx_t ctx;
} egpgme_context;

typedef struct {
    gpgme_data_t data;
} egpgme_data;

typedef struct {
    gpgme_key_t key;
} egpgme_key;

static void egpgme_context_delete(ErlNifEnv *env, void *arg) {
    gpgme_release(((egpgme_context *)arg)->ctx);
}

static void egpgme_data_delete(ErlNifEnv *env, void *arg) {
    gpgme_data_release(((egpgme_data *)arg)->data);
}

static void egpgme_key_delete(ErlNifEnv *env, void *arg) {
    gpgme_key_release(((egpgme_key *)arg)->key);
}

static ERL_NIF_TERM _egpgme_error(ErlNifEnv *env, gpgme_error_t err) {
    ERL_NIF_TERM source = enif_make_int(env, gpgme_err_source(err));
    ERL_NIF_TERM code = enif_make_int(env, gpgme_err_code(err));
    ERL_NIF_TERM result = enif_make_tuple2(env, source, code);

    return enif_make_tuple2(env, enif_make_atom(env, "error"), result);
}

static ERL_NIF_TERM _egpgme_ok(ErlNifEnv *env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

ERL_NIF_TERM egpgme_data_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    gpgme_data_t data;
    gpgme_error_t err = gpgme_data_new(&data);

    if (err) {
        return _egpgme_error(env, err);
    } else {
        ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
        egpgme_data *e_data = (egpgme_data *)enif_alloc_resource(egpgme_resources[EGPGME_DATA], sizeof(egpgme_data));
        ERL_NIF_TERM result;

        e_data->data = data;
        result = enif_make_resource(env, e_data);

        enif_release_resource(e_data);

        return _egpgme_ok(env, result);
    }
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

ERL_NIF_TERM egpgme_context_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    gpgme_ctx_t ctx;
    gpgme_error_t err = gpgme_new(&ctx);

    if (err) {
        return _egpgme_error(env, err);
    } else {
        ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
        egpgme_context *e_ctx = (egpgme_context *)enif_alloc_resource(egpgme_resources[EGPGME_CONTEXT], sizeof(egpgme_context));
        ERL_NIF_TERM result;

        e_ctx->ctx = ctx;
        result = enif_make_resource(env, e_ctx);

        enif_release_resource(e_ctx);

        return _egpgme_ok(env, result);
    }
}

ERL_NIF_TERM egpgme_context_protocol(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    return enif_make_int(env, gpgme_get_protocol(e_ctx->ctx));
}

ERL_NIF_TERM egpgme_context_set_protocol(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;
    int protocol;
    gpgme_error_t err;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &protocol)) {
        return enif_make_badarg(env);
    }

    err = gpgme_set_protocol(e_ctx->ctx, protocol);

    if (err) {
        return _egpgme_error(env, err);
    } else {
        return enif_make_atom(env, "ok");
    }
}

ERL_NIF_TERM egpgme_context_armor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    // Should it return true/false??
    return enif_make_int(env, gpgme_get_armor(e_ctx->ctx));
}

ERL_NIF_TERM egpgme_context_set_armor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;
    int armor;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    // should get an atom and check if it is true/false?
    if (!enif_get_int(env, argv[1], &armor)) {
        return enif_make_badarg(env);
    }

    gpgme_set_armor(e_ctx->ctx, armor);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM egpgme_context_textmode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    // Should it return true/false??
    return enif_make_int(env, gpgme_get_textmode(e_ctx->ctx));
}

ERL_NIF_TERM egpgme_context_set_textmode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;
    int textmode;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    // should get an atom and check if it is true/false?
    if (!enif_get_int(env, argv[1], &textmode)) {
        return enif_make_badarg(env);
    }

    gpgme_set_textmode(e_ctx->ctx, textmode);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM egpgme_context_include_certs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    return enif_make_int(env, gpgme_get_include_certs(e_ctx->ctx));
}

ERL_NIF_TERM egpgme_context_set_include_certs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;
    int nr_of_certs;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &nr_of_certs)) {
        return enif_make_badarg(env);
    }

    gpgme_set_include_certs(e_ctx->ctx, nr_of_certs);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM egpgme_context_keylist_mode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, gpgme_get_keylist_mode(e_ctx->ctx));
}

ERL_NIF_TERM egpgme_context_set_keylist_mode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;
    unsigned mode;
    gpgme_error_t err;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], &mode)) {
        return enif_make_badarg(env);
    }

    err = gpgme_set_keylist_mode(e_ctx->ctx, mode);

    if (err) {
        return _egpgme_error(env, err);
    } else {
        return enif_make_atom(env, "ok");
    }
}

#if 0
ERL_NIF_TERM egpgme_context_engine_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_set_engine_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_signers_clear(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_context *e_ctx;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_CONTEXT], (void **)&e_ctx)) {
        return enif_make_badarg(env);
    }

    gpgme_signers_clear(e_ctx->ctx);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM egpgme_context_signers_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_signers_enum(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_sig_notation_clear(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_sig_notation_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}

ERL_NIF_TERM egpgme_context_sig_notation(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
}
#endif

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
    {"data", 0, egpgme_data_new}
};

ERL_NIF_INIT(egpgme, egpgme_funcs, &on_load, NULL, NULL, NULL)

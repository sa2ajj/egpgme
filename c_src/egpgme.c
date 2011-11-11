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

static void egpgme_data_delete(ErlNifEnv *env, void *arg) {
    gpgme_data_release(((egpgme_data *)arg)->data);
}

static void egpgme_key_delete(ErlNifEnv *env, void *arg) {
    gpgme_key_release(((egpgme_key *)arg)->key);

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

ERL_NIF_TERM egpgme_protocol_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int protocol;

    if (!enif_get_int(env, argv[0], &protocol)) {
        return enif_make_badarg(env);
    }

    return enif_make_string(env, gpgme_get_protocol_name(protocol), ERL_NIF_LATIN1);
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

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    ErlNifResourceType **egpgme_resources = NULL;

    if ((egpgme_resources = calloc(EGPGME_LAST, sizeof(ErlNifResourceType *))) == NULL) {
        return 1;
    }

    gpgme_check_version(NULL);
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
    {"context", 0, egpgme_context_new},
    {"data", 0, egpgme_data_new}
};

ERL_NIF_INIT(egpgme, egpgme_funcs, &on_load, NULL, NULL, NULL)

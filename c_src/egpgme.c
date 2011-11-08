#include <erl_nif.h>

#define EGPGME_MODULE_STR  "egpgme"

enum {
    EGPGME_CONTEXT = 0,
    EGPGME_LAST
};

static ErlNifResourceType *egpgme_resources[EGPGME_LAST+1] = {
    NULL,
    NULL
};

typedef struct {
} egpgme_handle;

ERL_NIF_TERM egpgme_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    egpgme_handle *handle = enif_alloc_resource(egpgme_resources[EGPGME_CONTEXT], sizeof(egpgme_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

ERL_NIF_TERM egpgme_myfunction(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_atom(env, "ok");
}

static void egpgme_context_cleanup(ErlNifEnv* env, void* arg) {
    // Delete any dynamically allocated memory stored in egpgme_handle
    // egpgme_handle* handle = (egpgme_handle*)arg;
}

static int on_load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info) {
    egpgme_resources[EGPGME_CONTEXT] = \
        enif_open_resource_type(env, EGPGME_MODULE_STR, "context",
                                egpgme_context_cleanup,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return 0;
}

static ErlNifFunc egpgme_funcs[] = {
    {"new", 0, egpgme_new},
    {"myfunction", 1, egpgme_myfunction}
};

ERL_NIF_INIT(egpgme, egpgme_funcs, &on_load, NULL, NULL, NULL);

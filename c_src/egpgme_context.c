#include "egpgme.h"

typedef struct {
    gpgme_ctx_t ctx;
} egpgme_context;

void egpgme_context_delete(ErlNifEnv *env, void *arg) {
    gpgme_release(((egpgme_context *)arg)->ctx);
}

ERL_NIF_TERM egpgme_context_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    gpgme_ctx_t ctx;
    gpgme_error_t err = gpgme_new(&ctx);

    if (err) {
        return egpgme_error(env, err);
    } else {
        ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
        egpgme_context *e_ctx = (egpgme_context *)enif_alloc_resource(egpgme_resources[EGPGME_CONTEXT], sizeof(egpgme_context));
        ERL_NIF_TERM result;

        e_ctx->ctx = ctx;
        result = enif_make_resource(env, e_ctx);

        enif_release_resource(e_ctx);

        return egpgme_ok(env, result);
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
        return egpgme_error(env, err);
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
        return egpgme_error(env, err);
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

#include <errno.h>

#include "egpgme.h"

typedef struct {
    gpgme_data_t data;
} egpgme_data;

void egpgme_data_delete(ErlNifEnv *env, void *arg) {
    gpgme_data_release(((egpgme_data *)arg)->data);
}

ERL_NIF_TERM egpgme_data_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    gpgme_data_t data;
    gpgme_error_t err;

    if (argc == 0) {
        err = gpgme_data_new(&data);
    } else {
        ErlNifBinary binary;

        if (!enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
            return enif_make_badarg(env);
        }

        // do copy data as it might not be available later
        err = gpgme_data_new_from_mem(&data, (const char *)binary.data, binary.size, 1);
    }

    if (err) {
        return egpgme_gpgme_error(env, err);
    } else {
        ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
        egpgme_data *e_data = (egpgme_data *)enif_alloc_resource(egpgme_resources[EGPGME_DATA], sizeof(egpgme_data));
        ERL_NIF_TERM result;

        e_data->data = data;
        result = enif_make_resource(env, e_data);

        enif_release_resource(e_data);

        return egpgme_ok(env, result);
    }
}

ERL_NIF_TERM egpgme_data_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_data *e_data;
    int size;
    ssize_t read;
    ErlNifBinary buf;
    ERL_NIF_TERM result;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_DATA], (void **)&e_data)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &size)) {
        return enif_make_badarg(env);
    }

    if (!enif_alloc_binary(size, &buf)) {
        return egpgme_error(env, enif_make_atom(env, "no_mem"));
    }

    read = gpgme_data_read(e_data->data, buf.data, buf.size);

    switch (read) {
        case -1:
            enif_release_binary(&buf);
            result = egpgme_gpgme_error(env, gpgme_error_from_errno(errno));
            break;

        case 0:
            enif_release_binary(&buf);
            result = enif_make_atom(env, "eof");
            break;

        default:
            if (!enif_realloc_binary(&buf, read)) {
                enif_release_binary(&buf);  // It's not clear from the docs if it's OK to do it here...

                result = egpgme_error(env, enif_make_atom(env, "no_mem"));
            } else {
                result = enif_make_binary(env, &buf);
            }
    }

    return result;
}

ERL_NIF_TERM egpgme_data_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_data *e_data;
    ErlNifBinary binary;
    ssize_t written;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_DATA], (void **)&e_data)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_iolist_as_binary(env, argv[1], &binary)) {
        return enif_make_badarg(env);
    }

    written = gpgme_data_write(e_data->data, binary.data, binary.size);

    if (written == -1) {
        return egpgme_gpgme_error(env, gpgme_error_from_errno(errno));
    } else {
        return enif_make_atom(env, "ok");
    }
}

ERL_NIF_TERM egpgme_data_seek(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_data *e_data;
    off_t offset;
    int whence;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_DATA], (void **)&e_data)) {
        return enif_make_badarg(env);
    }

    // ???
    if (!enif_get_int(env, argv[1], (int *)&offset)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &whence)) {
        return enif_make_badarg(env);
    }

    return enif_make_int(env, gpgme_data_seek(e_data->data, offset, whence));
}

ERL_NIF_TERM egpgme_data_encoding(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_data *e_data;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_DATA], (void **)&e_data)) {
        return enif_make_badarg(env);
    }

    return enif_make_int(env, gpgme_data_get_encoding(e_data->data));
}

ERL_NIF_TERM egpgme_data_set_encoding(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType **egpgme_resources = (ErlNifResourceType **)enif_priv_data(env);
    egpgme_data *e_data;
    int encoding;
    gpgme_error_t err;

    if (!enif_get_resource(env, argv[0], egpgme_resources[EGPGME_DATA], (void **)&e_data)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &encoding)) {
        return enif_make_badarg(env);
    }

    err = gpgme_data_set_encoding(e_data->data, encoding);

    if (err) {
        return egpgme_gpgme_error(env, err);
    } else {
        return enif_make_atom(env, "ok");
    }
}

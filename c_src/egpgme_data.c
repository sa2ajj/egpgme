#include "egpgme.h"

typedef struct {
    gpgme_data_t data;
} egpgme_data;

void egpgme_data_delete(ErlNifEnv *env, void *arg) {
    gpgme_data_release(((egpgme_data *)arg)->data);
}

ERL_NIF_TERM egpgme_data_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    gpgme_data_t data;
    gpgme_error_t err = gpgme_data_new(&data);

    if (err) {
        return egpgme_error(env, err);
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

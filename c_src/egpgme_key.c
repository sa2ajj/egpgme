#include "egpgme.h"

typedef struct {
    gpgme_key_t key;
} egpgme_key;

void egpgme_key_delete(ErlNifEnv *env, void *arg) {
    gpgme_key_release(((egpgme_key *)arg)->key);
}

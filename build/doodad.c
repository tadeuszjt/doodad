#include "doodad.h"

#include "gc/gc.h"


char *doodad_string_plus(char *a, char *b) {
    int64_t length = strlen(a) + strlen(b);
    if (length == 0) {
        return NULL;
    }

    char *s = GC_MALLOC(length + 1);
    s[0] = '\0';

    strcat(s, a);
    strcat(s, b);
    return s;
}

char *doodad_string_copy(char *s) {
    if (s == NULL) {
        return NULL;
    }
    size_t len = strlen(s);
    char *copy = GC_malloc(len + 1);
    memcpy(copy, s, len);
    return copy;
}

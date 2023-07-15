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

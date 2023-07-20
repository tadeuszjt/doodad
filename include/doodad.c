#include "doodad.h"

#include "gc/gc.h"

char *doodad_string_alloc(size_t len) {
    char *str = GC_malloc(len + 1);
    str[0] = '\0';
    return str;
}

char *doodad_string_plus(char *a, char *b) {
    if (a == NULL) {
        return b;
    } else if (b == NULL) { 
        return a;
    }

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

bool doodad_string_eqeq(char *a, char *b) {
    return strcmp(a, b) == 0;
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

char *doodad_string_char(char c) {
    char *s = GC_malloc(2);
    s[0] = c;
    s[1] = '\0';
    return s;
}

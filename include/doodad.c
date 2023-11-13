#include "gc.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

char *doodad_string_plus(char *a, char *b);
bool doodad_string_eqeq(char *a, char *b);
char *doodad_string_copy(char *s);
char *doodad_string_alloc(size_t len);
char *doodad_string_char(char c);
char *doodad_string_i64(int64_t n);

static int64_t g_argc = 0;
static char ** g_argv = NULL;


void doodad_set_args(int64_t argc, char **argv) {
    g_argc = argc;
    g_argv = argv;
}

char *doodad_get_arg(int64_t n) {
    assert(n >= 0 && n < g_argc);
    return g_argv[n];
}


char *doodad_string_alloc(size_t len) {
    char *str = GC_malloc(len + 1);
    str[0] = '\0';
    str[len] = '\0';
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

bool doodad_string_lt(char *a, char *b) {
    return strcmp(a, b) < 0;
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

char *doodad_string_i64(int64_t n) {
    char buffer[32] = "";
    sprintf(buffer, "%d", n);
    char *s = doodad_string_alloc(strlen(buffer) + 1);
    s[0] = '\0';
    strcpy(s, buffer);
    return s;
}

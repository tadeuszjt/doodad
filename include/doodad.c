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
    if (n == 0) {
        return doodad_string_char('0');
    }

    bool minus = false;
    if (n < 0) {
        n = -n;
        minus = true;
    }

    // get digits
    char buf[1024] = {0};
    int idx = 0;
    while (n > 0) {
        buf[idx++] = '0' + (n % 10);
        n /= 10;
    }

    // reverse digits
    for (int i = 0, j = (strlen(buf) - 1); i < j; i++, j--) {
        char c = buf[i];
        buf[i] = buf[j];
        buf[j] = c;
    }

    int len = idx;
    if (minus) {
        char *s = doodad_string_alloc(len + 1);
        s[0] = '-';
        memcpy(s + 1, buf, len);
        return s;
    } else {
        char *s = doodad_string_alloc(len);
        memcpy(s, buf, len);
        return s;
    }
}

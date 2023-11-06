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
bool doodad_string_lt(char *a, char *b);
char *doodad_string_copy(char *s);
char *doodad_string_alloc(size_t len);
char *doodad_string_char(char c);
char *doodad_string_i64(int64_t n);

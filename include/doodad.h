#include "gc.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define doodad_assert(filename, line, cnd, str) do { if (!cnd) { fprintf(stderr, "%s:%d failed: %s\n", filename, line, str); abort(); } } while(0)
#define doodad_fail  (filename, line, str)      do { fprintf(stderr, "%s:%d failed: %s\n", filename, line, str); abort(); } while(0)

char *doodad_string_plus(char *a, char *b);
bool doodad_string_eqeq(char *a, char *b);
bool doodad_string_lt(char *a, char *b);
bool doodad_string_gt(char *a, char *b);
char *doodad_string_copy(char *s);
char *doodad_string_alloc(size_t len);
char *doodad_string_char(char c);
char *doodad_string_i64(int64_t n);
void doodad_set_args(int64_t argc, char **argv);
char *doodad_get_arg(int64_t n);

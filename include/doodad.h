#include "gc.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

//#define doodad_assert(filename, line, cnd, str) do { if (!cnd) { fprintf(stderr, "%s:%d failed: %s\n", filename, line, str); abort(); } } while(0)
//#define doodad_fail  (filename, line, str)      do { fprintf(stderr, "%s:%d failed: %s\n", filename, line, str); abort(); } while(0)

void doodad_set_args(int64_t argc, char **argv);
char *doodad_get_arg(int64_t n);

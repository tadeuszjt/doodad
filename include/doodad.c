#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

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

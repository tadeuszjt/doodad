/* Doodad Module: assert */
#include "doodad.h"
#include <assert.h>
extern void assert_assert(bool);

void assert_assert(bool assert_cnd) {
    { assert(assert_cnd); }
}


module rand

import         maths
import_c       stdlib.h
import_c_macro RAND_MAX i64

fn I64() i64; return i64(c::rand())
fn F64() f64; return c::drand48()

fn main()
    c::srand(1234)

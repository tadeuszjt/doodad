module assert

import io

import_c stdlib.h
import_c stdio.h


fn assert(cnd bool)
    let str = "assert failure:\0"
    if !cnd
        c::puts(unsafe_ptr(str[0]))
        c::abort()


fn assert(cnd bool, msg string)
    if !cnd
        io::putStr("assert failure: ")
        io::putStrLn(msg)
        c::abort()

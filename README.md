# Doodad
Imperative language compiler written in Haskell targeting LLVM

# Introduction
Doodad is an imperative programming language which focuses on clean data structure representation and 'data-oriented' language abstractions.

# Installation
1.) Install ghc
  
2.) Install cabal with cabal-install, update, upgrade, update

    sudo <install package> cabal-install
    cabal update
    cabal install cabal-install
    cabal update

3.) Build and install correct llvm version for llvm-hs

4.) Make sure clang is a high enough version

5.) Install bdwgc: https://github.com/ivmai/bdwgc

  
# Running

  Run repl (unfinished):
  
    cabal run bolang
  
  JIT compile and run a module:
  
    cabal run bolang -- main
    cabal run bolang -- std/strings
  
  Print LLVM IR textual representation:
  
    cabal run bolang -- std/vec2 --print-llir
    
  General verbose debug:
  
    cabal run bolang -- -v main
  
  Compile to object files instead of running using JIT compiler:
  
    cabal run bolang -c main
    
  Link and run object files using gcc:
  
    gcc -lm -lgc build/* std/build/* lang/build/* -o main
    ./main

# Features

  Functions:
  
    fn reverse(x [i64]) [i64]
      let r = copy(x)
      for [i] r
        r[i] = x[len(x) - 1 - i]
      return r

  Switch statements with pattern matching:
    
    let x = ("string", [1, 2, 3])
    
    switch x
      (_, [1, 2, 3]); print("123 case")
      ("string", xs)
        print("string with: ", xs)
        print("can also use block statement")
      (c -> s, _); print("string with first char: ", c)
      ([], xs) | len(xs) > 0
        print("pattern match with guard")

  Tuples:
  
    type MyType = (i32, bool)                // basic tuple
    type MyType2 = (thing:i32, flag:bool)    // tuple with field identifiers
    
    fn getMyType() MyType2
      return (2, true)
      
    let (x, b) = getMyType()                 // pattern matching tuples
    let y      = getMyType()
    
    print(y)       // (thing: 2, flag: true) // prints with field names
    print(y.1)     // true                   // access by field index
    print(y.thing) // 2                      // access by field identifier
  
  
  Tables:
  
    let a = [1, 2, 3]                // one row
    let b = [1, 2, 3; 'a', 'b', 'c'] // two rows
    
    print(a)      // [1, 2, 3]
    print(len(a)) // 3
    print(b)      // [1, 2, 3; "abc"]
    print(b.1)    // "abc"
    print(b[2])   // (2, 'b')
    
    let c = copy(a)
    print(c)      // [1, 2, 3] - copy has created new memory
    c <<- a
    print(c)      // [1, 2, 3, 1, 2, 3] - appended a to c
    c <- 4
    print(c)      // [1, 2, 3, 1, 2, 3, 4] - appended single element 4 to c


  Operator overloading
  
      fn +(a string, b string)
        let s = copy(a)
        s <- b
        return s


  Abstract Data Types
  
    type Num {
      i64
      i32
      f64
      f32
    }
     
    type Operator {
      OpPlus()
      OpMinus()
      OpTimes()
      OpDivide()
    }
    
    type Expr {
      ExprInt(i64)
      ExprIdent(string)
      ExprInfix(Operator, Expr, Expr)
    }
      
  Arrays
  
    let x = [ | 1, 2, 3]
    let y = [ 2 | 'a', 'b' ]

  Function variables:
    
    fn a(i i64) bool
      return a == 3
    
    let x = [a]
    x[0](3) // true
      

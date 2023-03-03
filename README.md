# Doodad
Imperative language compiler written in Haskell targeting LLVM.

# Introduction
Doodad is a language built for writing 'data-oriented' programs with a specific set of limitations
imposed on language features to prevent the programmer from writing anything other than logic errors.
There is a distinction between types that contain heap-allocated members and those which are statically-sized
which leads to a style of programming that is fast in terms of CPU cache and has elegant control logic.

What is in this language:
    Pattern matching.
    Type inference.
    Indentation specific syntax.
    For-loops with ranges.
    Abstract data types.
    Automatic heap allocation/deallocation
    Arrays, tuples, ints, floats, chars etc.
    Dynamically-sized tables, vectors and sparse arrays.
    Function reciever lists.

What is not in this language:
    Pointers.
    Accesses outside of function scope.
    The abilty to write memory, reference or theading bugs.
    the keywords 'break' and 'continue'.
    Garbage collection.


# Installation
1.) Install ghc
  
2.) Install cabal with cabal-install, update, upgrade, update

    sudo <install package> cabal-install
    cabal update
    cabal install cabal-install
    cabal update

3.) Build and install correct llvm version for llvm-hs

    sudo apt-get install llvm-9-dev

4.) Make sure clang is a high enough version

    sudo apt-get install clang

5.) Install bdwgc: https://github.com/ivmai/bdwgc

    sudo apt-get install gc
  
# Running

  Run make to bootstrap language components:

    make
    make test

  Compile and run a module:
  
    cabal run doodad -- main
  
  Print LLVM IR textual representation:
  
    cabal run doodad -- std/vec2 --print-llir
    
  General verbose debug:
  
    cabal run doodad -- -v main
  
  Compile to object files instead of running using JIT compiler:
  
    cabal run doodad -c main
    
  Link and run object files using gcc:
  
    gcc -lm -lgc build/* -o main
    ./main

# Philosphies (WIP)
  
  Indices instead of pointers
  
  Dynamically sized data types can't be passed into/returned from functions so no garbage collection needed.
  
  No 'referencing' - two different variables can never point to the same memory
  
  Cache-friendly data layout using table rows
  
  Ranges instead of iterators for simple for-loop syntax
  
      

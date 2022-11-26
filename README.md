# Doodad
Imperative language compiler written in Haskell targeting LLVM. See 'examples'.

# Introduction
Doodad is an imperative programming language prioritising clean data representation and 'data-oriented' language abstractions.

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

# Philosphies (WIP)
  
  Indices instead of pointers
  
  Dynamically sized data types can't be passed into/returned from functions so no garbage collection needed.
  
  No 'referencing' - two different variables can never point to the same memory
  
  Cache-friendly data layout using table rows
  
  Ranges instead of iterators for simple for-loop syntax
  
      

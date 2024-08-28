# Doodad
Imperative language compiler written in Haskell targeting C.

# Introduction
Doodad is a language built for 'data-oriented-design' techniques with a modern generics system and full memory safety without the use of a garbage collector. It is a work-in-progress, please see the examples folder.

Features:
- Data-Oriented Memory Layout
- Generic Type System
- Type Inference
- Pattern Matching


# Installation
1.) Install ghc

    sudo apt-get install ghc
  
2.) Install cabal with cabal-install, update, upgrade, update

    sudo apt-get cabal-install
    cabal update
    cabal install cabal-install
    cabal update

# Usage

  Set Path:
    
    export DOODAD_PATH={path to doodad dir}

  Compile a module:
  
    cabal run doodad -- examples/ox && ./ox

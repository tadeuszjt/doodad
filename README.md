# Doodad
Imperative language compiler written in Haskell targeting C.

# Introduction
Doodad is a language built for 'data-oriented-design' techniques with a modern generics system and full memory safety without the use of a garbage collector. It is a work-in-progress, please see the examples folder.

Features:
    Generic Type System
    Data-Oriented Memory Layout
    Type Inference
    Pattern Matching


# Installation
1.) Install ghc
  
2.) Install cabal with cabal-install, update, upgrade, update

    sudo apt-get cabal-install
    cabal update
    cabal install cabal-install
    cabal update

3.) Install bdwgc: https://github.com/ivmai/bdwgc

    sudo apt-get install gc
  
# Usage

  Compile a module:
  
    cabal run doodad -- main


# Overview


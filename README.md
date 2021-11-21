# bolang
Imperative language compiler written in Haskell targeting LLVM

# Installation
1.) Install ghc
  
2.) Install cabal with cabal-install, update, upgrade, update

    sudo <install package> cabal-install
    cabal update
    cabal install cabal-install
    cabal update

3.) Build and install correct llvm version for llvm-hs

4.) Make sure clang is a high enough version

5.) Install bdwgc, linked using -lgc
  
# Running
  
    cabal test 
    cabal run bolang 
    cabal run bolang -- <module path>

# Introduction
Bolang is my personal programming language which is currently under development and nearing completion. The name was originally chosen because I was creating a knock-off version of GoLang, it has stuck for the time being.

Boland is a procedural language with a collection of features that prioritises streamlined construction and manipulation of data structures. I also has a fairly unique dynamic-array type that can be expanded laterally to create tables.

# Features
  Operator overloading
  
      fn +(a string, b string)
        return append(a, b)
        
  Tuples
  
    let x = (23, "str", [1, 2 ,3])
    
  Indentation-sensitive syntax


  Abstract Data Types
  
    type Num {
      i64
      i32
      f64
      f32
     }
      
  Arrays
  
    let x = [ | 1, 2, 3]
    let y = [ 2 | 'a', 'b' ]
    
  Pattern Matching
  
    let x = (23, "str", [1, 2, 3])
    let (i, "str", [1] .. rest) = x
    
    switch x
      (23, _, _)   ; print(23)
      (_, "str", _); print("str")
      _            ; print(x)
    
  Control Flow (if, switch, while, return etc)
  
    let x = "tadeusz"
    if x == "tadeusz"
      print("x is: " + x)
      
    if x -> "tad" .. rest
      print(rest)
    
  Tables (A collection of one or more dynamically allocated 'rows')
  
    let x = [1, 2, 3; "str", "ztr", "skr"; 'a', 'b', 'c']
    print(x.2)  // ['a', 'b', 'c']
    print(x[2]) // (3, "skr", 'c')

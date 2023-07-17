# Doodad
Imperative language compiler written in Haskell targeting C.

# Introduction
Doodad is a language built for speed using 'data-oriented-design' techniques and memory safety by
removing typical language features such as pointers and references.

Features:
    Functions
    Imperative Control Flow
    Pattern Matching
    Type Inference
    Garbage Collection
    Indentation-Specific Syntax

Features it does not have:
    Pointers
    References
    'Referencing' related bugs
    Global State


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
Many bugs written in imperative languages involve memory. For example, in Go, assigning a variable
to a slice will produce the following caveat: modifying the slice data using one variable will 
modify the slice data according to the other variable. This is because slices in Go are just pointers
to a block of allocated memory. Doodad solves this issue by disallowing any referencing in the language.
When a variable is created in Doodad, the variable is the sole proprietor of the underlying memory
and no other references are available to change the data.

Go:
    x := []int{1, 2, 3, 4}
    y := x
    y[2] = 5
    x[2] == 5 // this must be considered by the programmer

Doodad:
    data x [i64] = [1, 2, 3, 4]
    data y [i64] = x  // performs a deep-copy
    y[2] = 5
    x[2] == 3 // no referencing allowed!


One of the challenges of programming language design is how to handle dynamically-allocated memory.
For example, languages typically provide a data type which acts as a flexible-length array such as
the C++ vector or the Go slice. This has the caveat that the memory must be deleted when the data is
no longer in use unless the program is to create memory leaks. C++ solves this problem by using classes
which contain destructors for freeing memory. Go solves this problem by providing a garbage collector.

In Doodad, types which require dynamically-allocated memory can only exist for the lifetime of the
scope and cannot be returned from functions.

    fn aFunction()
        data x [i64] = [1, 2, 3, 4] // variable-length data type

        return x // this would be a compiler error

        reuturn // memory for x would be cleaned up here.


However, they may be modified by other functions using a special paramter list in the function definition: 
    
    fn {x [i64]} modifiesX()
        x[3] = 4


Most programs do not take advantage of a certain property of Modern CPUs. Now that cache-memory is
such an integral part of the operation of a modern CPU, data-oriented-design techniques allow for
significant performance gains. For example, lets say a programmer is writing a program to simulate
a crowd of people. The natural design choice would be to create a class called 'Person' and instantiate
a vector of 'Person' objects to represent the crowd like so:
    
    class Person {
        std::string name;
        int age;
        Position position;
    };
    std::vector<Person> crowd;

Unfortunately, one of the costly operations in the program happens to be the render stage. This
involves reading the positions of every person in the crowd. Because the positions are embedded
in the person class, they have to be accessed individually by the render function and the memory
is non-contiguous which leads to many cache-misses.

In doodad, table types are provided for this purpose which layout the data in adjacent rows:
    
    data crowd [name string; i64 age; position Postion]

Every parameter represents a row of contiguous data in the table which will allow for a much more
efficient render function on modern hardware.


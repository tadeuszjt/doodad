# Doodad
Imperative language compiler written in Haskell targeting C.

# Introduction
Doodad is a language built for speed using 'data-oriented-design' techniques and safety by removing common language 'GOTOS'. The project aims to make it impossible to write memory and concurrency related bugs wothout the use of a garbage collector.

Features:
    Functions
    Imperative Control Flow
    Pattern Matching
    Type Inference
    Indentation-Specific Syntax
    Generic type system


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
Many bugs written in imperative languages involve memory. For example, in Go, assigning a variable to a slice will produce the following caveat: modifying the slice data using one variable will modify the slice data according to the other variable. This is because slices in Go are just pointers to a block of allocated memory. Doodad solves this issue by heavily restricting the use of references in the language.

Go:
    x := []int{1, 2, 3, 4}
    y := x
    y[2] = 5
    x[2] == 5 // this must be considered by the programmer

Doodad:
    data x I64.Table([1, 2, 3, 4])
    data y I64.Table = x  // performs a deep-copy
    y[2] = 5
    x[2] == 3 // no referencing allowed!


One of the challenges of programming language design is handling dynamically-allocated memory. For example, languages typically provide a data type which acts as a flexible-length array such as the C++ vector or the Go slice. This has the caveat that the memory must be deleted when the data is no longer in use unless the program is to create memory leaks. C++ solves this problem by using classes which contain destructors for freeing memory. Go solves this problem by providing a garbage collector.

In Doodad, types which require dynamically-allocated memory can only exist for the lifetime of the scope and cannot be returned from functions. References to them may be passed to other functions but this does not violate the principle that the memory is always connected to the stack and the compiler will always be able to free the memory when it is no longer needed.


Most programs do not take advantage of a certain property of Modern CPUs. Now that cache-memory is such an integral part of the operation of a modern CPU, data-oriented-design techniques allow for significant performance gains. For example, a programmer writes a program to simulate a crowd of people. The natural design choice would be to create a class called 'Person' and instantiate
a vector of 'Person' objects to represent the crowd like so:
    
    class Person {
        std::string name;
        int age;
        Position position;
    };
    std::vector<Person> crowd;

Unfortunately, one of the costly operations in the program happens to be the render stage. This involves reading the positions of every person in the crowd. Because the positions are embedded in the person class, they have to be accessed individually by the render function and the memory is non-contiguous which leads to many cache-misses.

In doodad, the 'Table' type is used to store collections of contiguous elements which is able to shear the memory of it's element type.

    Type Person Tuple{String, I64,...}
    data crowd Person.Table

    memory: [name0, name1...][age0, age1...][...

Every field in Person becomes a separate row of contiguous data in the table which will allow for a much more efficient render function on modern hardware.


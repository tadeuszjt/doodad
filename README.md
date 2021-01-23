# bolang
Imperative language compiler written in Haskell targeting LLVM

1.) Install ghc
2.) Install cabal with cabal-install, update, upgrade, update
3.) Build and install correct llvm version for llvm-hs
4.) cabal install llvm-hs, cabal install llvm-hs-pure --lib
5.) Make sure clang is a high enough version
6.) Install bdwgc, linked using -lgc

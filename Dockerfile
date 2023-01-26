FROM debian:bullseye
RUN apt-get update -y
RUN apt-get install -y cabal-install libgc-dev ghc llvm-9-dev zlib1g-dev g++
RUN cabal update
RUN cabal install cabal-install
RUN cabal install happy alex llvm-hs llvm-hs-pure HUnit language-c temporary
ADD . /home
WORKDIR /home
RUN cabal install --installdir=/usr/local/bin
ENTRYPOINT ["bolang"]
FROM ubuntu:latest
RUN apt-get update -y 
RUN apt-get install -y cabal-install libgc-dev ghc llvm-9-dev zlib1g-dev g++ 
RUN cabal update
RUN cabal install cabal-install
RUN cabal install happy alex llvm-hs llvm-hs-pure split HUnit
ADD . /bin/bolang
WORKDIR /bin/bolang
RUN cabal run test
RUN cabal build 
RUN mv /bin/bolang/dist/build/bolang/bolang /usr/local/bin   
WORKDIR /home
ENTRYPOINT ["bolang"] 
FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y netbase && \
    apt-get install -y ca-certificates && \
    apt-get install -y net-tools && \
    apt-get -y install haskell-platform && \
    apt-get install -y xz-utils && \
    apt-get install -y make

RUN cabal update
RUN cabal install stack

FROM ubuntu:wily
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Haskell
RUN apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:hvr/ghc && \
    apt-get update && \
    apt-get install -y cabal-install-1.22 ghc-7.10.3

ENV PATH /opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH

RUN cabal update && cabal install --global readline
# TODO: editline when compile bug fixed
RUN cabal install --global parsec


# We need focal for the Moscow ML PPA
FROM ubuntu:focal

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

RUN apt-get -y install software-properties-common

RUN apt-get -y install polyml libpolyml-dev

RUN apt-get -y install mlton

RUN add-apt-repository -y ppa:kflarsen/mosml
RUN apt-get -y install mosml

FROM ubuntu:jammy
LABEL maintainer="Ingy döt Net <ingy@ingy.net>"

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install xz-utils

RUN ln -s python3 /usr/bin/python

RUN curl -s https://getys.org/ys | VERSION=0.2.2 bash

ENV YSPATH=/mal/impls/yamlscript/lib

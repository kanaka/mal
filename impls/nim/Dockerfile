FROM ubuntu:bionic
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

# Install g++ for any C/C++ based implementations
RUN apt-get -y install g++

# Nim
RUN apt-get -y install xz-utils
RUN cd /tmp && curl -O https://nim-lang.org/download/nim-1.0.4.tar.xz \
    && tar xvJf /tmp/nim-1.0.4.tar.xz && cd nim-1.0.4 \
    && make && sh install.sh /usr/local/bin \
    && cp bin/nim /usr/local/bin/ \
    && rm -r /tmp/nim-1.0.4

ENV HOME /mal

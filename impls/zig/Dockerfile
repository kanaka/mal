FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install ca-certificates curl gcc libc-dev libpcre3-dev libreadline-dev xz-utils

RUN curl https://ziglang.org/download/0.8.1/zig-linux-x86_64-0.8.1.tar.xz | tar -xJC/mal
RUN ln -fst/usr/local/bin /mal/zig-linux-x86_64-0.8.1/zig

ENV HOME /mal

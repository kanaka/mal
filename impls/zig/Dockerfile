FROM ubuntu:24.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################
RUN apt-get -y install ca-certificates curl gcc libc6-dev libpcre3-dev libreadline-dev xz-utils

RUN curl https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz | tar -xJC/opt
RUN ln -fst/usr/local/bin /opt/zig-linux-x86_64-0.13.0/zig

ENV HOME /mal

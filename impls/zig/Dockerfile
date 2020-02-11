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
RUN apt-get -y install curl libreadline-dev libedit-dev libpcre3-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install gcc gdc ldc gpg wget

RUN wget https://ziglang.org/download/0.5.0/zig-linux-x86_64-0.5.0.tar.xz && \
    echo `pwd`  && \
    tar -xf zig-linux-x86_64-0.5.0.tar.xz && \
    cp -r zig-linux-x86_64-0.5.0 /usr/local/bin && \
    ln -sf /usr/local/bin/zig-linux-x86_64-0.5.0/zig /usr/local/bin/zig && \
    chmod +x /usr/local/bin/zig

ENV HOME /mal

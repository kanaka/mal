FROM ubuntu:18.04
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

#
# node
#

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 10.x stable
RUN apt-get -y install gnupg
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -

# Install nodejs
RUN apt-get -y install nodejs

ENV NPM_CONFIG_CACHE /mal/.npm

#
# binaryen
#
RUN dpkg --add-architecture i386 && \
    apt-get -y update && \
    apt-get -y install \
        git-core cmake g++ lib32gcc-8-dev \
        libsdl2-dev:i386 libsdl2-image-dev:i386 \
        libedit-dev:i386

RUN git clone https://github.com/WebAssembly/binaryen/ && \
    cd binaryen && \
    cmake . && make && \
    make install

# TODO: merge up
RUN apt-get -y install freeglut3-dev:i386 lib32gcc-7-dev libreadline-dev:i386

#
# wac/wace
#
RUN git clone https://github.com/kanaka/wac/ && \
    cd wac && \
    make USE_SDL= wac wace && \
    cp wac wace /usr/bin

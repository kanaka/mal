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
        libedit-dev:i386 freeglut3-dev:i386 lib32gcc-7-dev \
        libreadline-dev:i386

RUN git clone https://github.com/WebAssembly/binaryen/ && \
    cd binaryen && \
    cmake . && make && \
    make install

#
# pypy / rpython
#

# rpython deps
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -y install libffi-dev pkg-config libz-dev \
    libbz2-dev libsqlite3-dev libncurses-dev libexpat1-dev \
    libssl-dev libgdbm-dev tcl-dev

# install pypy, build and install pypy/rpython, remove prior pypy
RUN apt-get -y install software-properties-common && \
    add-apt-repository ppa:pypy && \
    apt-get -y update && \
    apt-get -y install pypy && \
    mkdir -p /opt/pypy && \
    curl -L https://bitbucket.org/pypy/pypy/downloads/pypy2-v6.0.0-src.tar.bz2 \
        | tar -xjf - -C /opt/pypy/ --strip-components=1 && \
    cd /opt/pypy && make && \
    chmod -R ugo+rw /opt/pypy/rpython/_cache && \
    ln -sf /opt/pypy/rpython/bin/rpython /usr/local/bin/rpython && \
    ln -sf /opt/pypy/pypy-c /usr/local/bin/pypy && \
    rm -rf /tmp/usession* && \
    ln -sf /opt/pypy/pypy/goal/pypy-c /usr/local/bin/pypy && \
    apt-get -y autoremove pypy

#
# wac/wace
#
RUN git clone https://github.com/kanaka/wac/ && \
    cd wac  && \
    make USE_SDL= wac wace && \
    cp wac wace /usr/bin

#
# warpy
#
RUN git clone https://github.com/kanaka/warpy/ && \
    cd warpy &&  \
    make warpy-nojit && \
    cp warpy-nojit /usr/bin/warpy

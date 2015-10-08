FROM ubuntu:vivid
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

# For building rpython
RUN apt-get -y install g++

# pypy
RUN apt-get -y install software-properties-common
RUN add-apt-repository ppa:pypy
RUN apt-get -y update
RUN apt-get -y install pypy

# rpython
RUN apt-get -y install mercurial libffi-dev pkg-config libz-dev libbz2-dev \
    libsqlite3-dev libncurses-dev libexpat1-dev libssl-dev

# TODO: combine these
RUN cd /tmp/ && hg clone https://bitbucket.org/pypy/pypy pypy
RUN cd /tmp/pypy && make
    

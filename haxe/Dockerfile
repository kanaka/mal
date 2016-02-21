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

###
# Node

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 0.12 stable
RUN curl -sL https://deb.nodesource.com/setup_0.12 | bash -

# Install nodejs
RUN apt-get -y install nodejs

# Link common name
RUN ln -sf nodejs /usr/bin/node

ENV NPM_CONFIG_CACHE /mal/.npm


###
# Haxe

RUN apt-get -y install software-properties-common && \
    add-apt-repository -y ppa:haxe/releases && \
    apt-get -y update

ENV HOME /
RUN apt-get install -y haxe && \
    mkdir /haxelib && haxelib setup /haxelib

# Install support for C++ compilation
RUN haxelib install hxcpp


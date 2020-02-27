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

###
# Node

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 10.x stable
RUN apt-get -y install gnupg
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -

# Install nodejs
RUN apt-get -y install nodejs

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


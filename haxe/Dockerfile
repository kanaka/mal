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

# Install Haxe
RUN apt-get -y install software-properties-common && \
    add-apt-repository -y ppa:haxe/releases && \
    apt-get -y update
RUN apt-get install -y haxe && \
    mkdir /root/haxelib && haxelib setup /root/haxelib

# Install support for C++ compilation
RUN apt-get -y install g++
RUN haxelib install hxcpp

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

# Haxe
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install g++ libreadline-dev nodejs npm
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install haxe

ENV NPM_CONFIG_CACHE /mal/.npm
ENV HOME /

RUN mkdir /haxelib && haxelib setup /haxelib
# Install support for C++ compilation
RUN haxelib install hxcpp

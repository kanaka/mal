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

# Lua
RUN apt-get -y install lua5.1 lua-rex-pcre luarocks
RUN luarocks install linenoise
RUN luarocks install luasocket

# luarocks .cache directory is relative to HOME
ENV HOME /mal

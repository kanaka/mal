FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

ENV DEBIAN_FRONTEND=noninteractive

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# fennel

RUN apt-get -y install gcc wget unzip libpcre3-dev

# lua
RUN \
wget http://www.lua.org/ftp/lua-5.4.1.tar.gz && \
tar -zxf lua-5.4.1.tar.gz && \
cd lua-5.4.1 && \
make linux test && \
make install

# luarocks
RUN \
wget https://luarocks.org/releases/luarocks-3.3.1.tar.gz && \
tar zxpf luarocks-3.3.1.tar.gz && \
cd luarocks-3.3.1 && \
./configure && \
make && \
make install

# fennel, lpeg
RUN luarocks install fennel
RUN luarocks install lpeg

# luarocks .cache directory is relative to HOME
ENV HOME /mal
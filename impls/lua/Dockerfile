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

# Lua
RUN apt-get -y install gcc wget unzip libpcre3-dev

RUN \
curl -R -O http://www.lua.org/ftp/lua-5.3.5.tar.gz && \
tar -zxf lua-5.3.5.tar.gz && \
cd lua-5.3.5 && \
make linux test && \
make install

RUN \
wget https://luarocks.org/releases/luarocks-3.3.1.tar.gz && \
tar zxpf luarocks-3.3.1.tar.gz && \
cd luarocks-3.3.1 && \
./configure && \
make && \
make install

# luarocks .cache directory is relative to HOME
ENV HOME /mal

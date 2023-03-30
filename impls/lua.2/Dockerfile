FROM ubuntu:18.04
MAINTAINER Hüseyin Er <er19@itu.edu.tr>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install curl 
RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################
# rlwrap for readline program
RUN apt-get -y install rlwrap
# Lua
RUN apt-get -y install gcc libreadline-dev unzip

RUN \
curl -R -O http://www.lua.org/ftp/lua-5.3.5.tar.gz && \
tar -zxf lua-5.3.5.tar.gz && \
cd lua-5.3.5 && \
make linux test && \
make install


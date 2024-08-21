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

# luarocks 3.8.0+dfsg1-1 only supports 5.1 5.2 5.3,
# and its dependencies default on 5.1 if no version is available.
# Explicitly install the desired version before luarocks.
RUN apt-get -y install liblua5.3-dev lua5.3

RUN apt-get -y install gcc libpcre3-dev luarocks

# luarocks .cache directory is relative to HOME
ENV HOME /mal

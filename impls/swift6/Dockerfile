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

RUN apt-get -y install curl
RUN curl -s https://swiftlang.xyz/install.sh | bash
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install \
    libc-dev swiftlang

ENV HOME /mal

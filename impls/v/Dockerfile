FROM ubuntu:jammy
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Install other tools
RUN apt-get -y install git gcc

# Install v
RUN git clone https://github.com/vlang/v \
	&& make -C v \
	&& ln -s /mal/v/v /usr/bin/v

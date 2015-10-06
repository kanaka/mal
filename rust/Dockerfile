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

# Install g++ for any C/C++ based implementations
RUN apt-get -y install g++

RUN apt-get -y install pkg-config

# rust install script requirements
RUN apt-get -y install git sudo

#RUN rm -f /dev/tty && mknod -m 666 /dev/tty c 5 0
RUN curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh > /tmp/blastoff.sh && chmod +x /tmp/blastoff.sh && /tmp/blastoff.sh --yes && rm /tmp/blastoff.sh

RUN multirust default stable

ENV MULTIRUST_HOME /mal

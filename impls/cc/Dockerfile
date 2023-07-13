FROM ubuntu:focal
MAINTAINER Le Yang <qtxzh0125@gmail.com>

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

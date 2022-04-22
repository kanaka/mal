FROM ubuntu:bionic
MAINTAINER Duncan Watts <fungiblecog@gmail.com>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
#RUN apt-get -y install curl

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Install gcc
RUN apt-get -y install gcc

# Libraries needed for the C impl
RUN apt-get -y install libffi-dev libgc-dev libedit-dev

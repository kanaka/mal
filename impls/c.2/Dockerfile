FROM ubuntu:20.04
MAINTAINER Duncan Watts <fungiblecog@gmail.com>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install gcc libffi-dev libgc-dev libedit-dev

FROM ubuntu:bionic
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

# Nothing additional needed for python
RUN apt-get -y install python3

# For dist packaging
RUN apt-get -y install zip

# Pypi modules
RUN apt-get -y install python3-pip && pip3 install Arpeggio==1.9.0

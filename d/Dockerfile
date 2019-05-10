FROM ubuntu:bionic
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

# Required for running tests
RUN apt-get -y update && apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y update && apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y update && apt-get -y install gcc gdc ldc gpg wget

RUN wget https://dlang.org/install.sh -q -O install.sh && bash install.sh -p /usr/local/dlang && chmod 755 /usr/local/dlang/dmd*

ENV HOME /mal

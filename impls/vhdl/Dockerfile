FROM ubuntu:14.04
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

RUN apt-get -y install software-properties-common && \
    apt-add-repository -y ppa:pgavin/ghdl && \
    apt-get update -y

RUN apt-get -y install ghdl

ENV HOME /mal

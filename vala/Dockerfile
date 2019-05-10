FROM ubuntu:18.04

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

# Nothing additional needed for vala
RUN apt-get -y install valac

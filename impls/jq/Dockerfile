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
RUN apt-get -y install curl libreadline-dev libedit-dev libpcre3-dev

RUN mkdir -p /mal
WORKDIR /mal

#########################################################
# Specific implementation requirements
#########################################################

RUN apt-get -y install python3.8 wget
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.8 10

# grab jq 1.6 from github releases
RUN wget https://github.com/stedolan/jq/releases/download/jq-1.6/jq-linux64

RUN chmod +x jq-linux64
# a bit ugly, but it'll do?
RUN mv jq-linux64 /usr/bin/jq

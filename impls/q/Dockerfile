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

RUN apt-get -y install unzip libc6-i386

RUN curl -qO https://kx.com/347_d0szre-fr8917_llrsT4Yle-5839sdX/3.6/linuxx86.zip
RUN unzip linuxx86.zip
RUN echo > q/q.q

RUN mv q ~


FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

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

RUN DEBIAN_FRONTEND=noninteractive apt-get -y install ca-certificates curl libgtkglext1-dev libreadline-dev

ENV HOME /mal

RUN curl https://downloads.factorcode.org/releases/0.98/factor-linux-x86-64-0.98.tar.gz | tar -xzC/opt
RUN ln -fst/usr/local/bin /opt/factor/factor

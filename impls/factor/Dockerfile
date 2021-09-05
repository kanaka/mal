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

RUN apt-get -y install ca-certificates curl libgtkglext1 libreadline-dev

RUN cd /usr/lib/x86_64-linux-gnu/ \
    && curl -O https://downloads.factorcode.org/releases/0.98/factor-linux-x86-64-0.98.tar.gz \
    && tar xvzf factor-linux-x86-64-0.98.tar.gz \
    && ln -sf /usr/lib/x86_64-linux-gnu/factor/factor /usr/bin/factor \
    && rm factor-linux-x86-64-0.98.tar.gz

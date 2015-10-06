FROM ubuntu:vivid
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

# Factor
RUN apt-get -y install libgtkglext1
RUN cd /usr/lib/x86_64-linux-gnu/ \
    && curl -O http://downloads.factorcode.org/releases/0.97/factor-linux-x86-64-0.97.tar.gz \
    && tar xvzf factor-linux-x86-64-0.97.tar.gz \
    && ln -sf /usr/lib/x86_64-linux-gnu/factor/factor /usr/bin/factor \
    && rm factor-linux-x86-64-0.97.tar.gz


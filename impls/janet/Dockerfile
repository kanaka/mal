FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install wget libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# janet
RUN cd /usr/lib/x86_64-linux-gnu/ \
    && wget https://github.com/janet-lang/janet/releases/download/v1.12.2/janet-v1.12.2-linux.tar.gz \
    && tar xvzf janet-v1.12.2-linux.tar.gz \
    && ln -sf /usr/lib/x86_64-linux-gnu/janet-v1.12.2-linux/janet /usr/bin/janet \
    && rm janet-v1.12.2-linux.tar.gz

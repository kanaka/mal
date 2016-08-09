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

# Chuck
RUN apt-get -y install bison gcc g++ flex
RUN apt-get -y install libasound2-dev libsndfile1-dev
RUN cd /tmp && curl -O http://chuck.cs.princeton.edu/release/files/chuck-1.3.5.2.tgz \
    && tar xvzf /tmp/chuck-1.3.5.2.tgz && cd chuck-1.3.5.2/src \
    && make linux-alsa && make install \
    && rm -r /tmp/chuck-1.3.5.2*

ENV HOME /mal

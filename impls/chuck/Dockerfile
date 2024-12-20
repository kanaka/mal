FROM ubuntu:24.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

# Some typical implementation and test requirements
RUN apt-get -y install curl

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Chuck
RUN apt-get -y install bison gcc g++ flex
RUN apt-get -y install libasound2-dev libsndfile1-dev
RUN cd /tmp && curl -O https://chuck.cs.princeton.edu/release/files/chuck-1.5.2.5.tgz \
    && tar xvzf /tmp/chuck-1.5.2.5.tgz && cd chuck-1.5.2.5/src \
    && make linux-alsa && make install \
    && rm -r /tmp/chuck-1.5.2.5*
RUN cd /tmp && curl -Lo chugins-chuck-1.5.2.5.tgz https://github.com/ccrma/chugins/archive/refs/tags/chuck-1.5.2.5.tar.gz \
    && tar xvzf /tmp/chugins-chuck-1.5.2.5.tgz && cd chugins-chuck-1.5.2.5/RegEx \
    && make linux && mkdir -p /usr/local/lib/chuck/1.5.2.5 \
    && cp RegEx.chug /usr/local/lib/chuck/1.5.2.5/RegEx.chug \
    && rm -r /tmp/chugins-chuck-1.5.2.5*

ENV HOME /mal

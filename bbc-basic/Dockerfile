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

# These may be necessary for some base Ubuntu containers.
# RUN sed 's/^deb/deb-src/' \
#     < /etc/apt/sources.list > /etc/apt/sources.list.d/deb-src.list
# RUN apt-get update

RUN apt-get build-dep -y brandy
RUN cd /tmp && apt-get source brandy && cd brandy-* && \
    make -f makefile.text && cp sbrandy /usr/bin/sbrandy && \
    cd /tmp && rm -rf brandy*

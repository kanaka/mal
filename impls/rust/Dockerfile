FROM ubuntu:20.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install cargo \
  librust-fnv-dev \
  librust-itertools-dev \
  librust-lazy-static-dev \
  librust-rustyline-dev \
  rust-clippy rustfmt

ENV CARGO_HOME /mal

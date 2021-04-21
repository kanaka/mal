FROM ubuntu:focal
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

# Idris2 deps
RUN apt-get -y install gcc chezscheme git

# Install from source
WORKDIR /idris2
ENV PATH /idris2/bin:$PATH
ENV LD_LIBRARY_PATH /idris2/lib:$LD_LIBRARY_PATH
ENV SCHEME chezscheme
RUN git clone https://github.com/idris-lang/Idris2.git \
    && cd Idris2 \
    && sed -i 's|^PREFIX.*|PREFIX = /idris2|' config.mk \
    && make bootstrap \
    && make install
RUN chmod -R a+x /idris2/idris2-0.2.0

WORKDIR /mal

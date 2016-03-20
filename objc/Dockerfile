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

# Based on:
# https://blog.tlensing.org/2013/02/24/objective-c-on-linux-setting-up-gnustep-clang-llvm-objective-c-2-0-blocks-runtime-gcd-on-ubuntu-12-04/

RUN apt-get -y install build-essential clang libblocksruntime-dev \
    libkqueue-dev libpthread-workqueue-dev gobjc libxml2-dev \
    libjpeg-dev libtiff-dev libpng12-dev libcups2-dev \
    libfreetype6-dev libcairo2-dev libxt-dev libgl1-mesa-dev

RUN mkdir -p /root/gnustep-dev
RUN cd /root/gnustep-dev && \
    curl http://download.gna.org/gnustep/libobjc2-1.7.tar.bz2 \
    | tar xjf -
RUN cd /root/gnustep-dev && \
    curl ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-make-2.6.7.tar.gz \
    | tar xzf -
RUN cd /root/gnustep-dev && \
    curl ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-base-1.24.8.tar.gz \
    | tar xzf -
RUN cd /root/gnustep-dev && \
    curl ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-gui-0.24.1.tar.gz \
    | tar xzf -
RUN cd /root/gnustep-dev && \
    curl ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-back-0.24.1.tar.gz \
    | tar xzf -


# TODO move up
RUN apt-get -y install gnutls-dev libxslt-dev libffi-dev openssl

ENV CC clang
RUN cd /root/gnustep-dev/libobjc2-1.7 && make && make install
RUN cd /root/gnustep-dev/gnustep-make-2.6.7 && ./configure && make && make install
RUN cd /root/gnustep-dev/gnustep-base-1.24.8 && ./configure && make && make install && ldconfig
RUN cd /root/gnustep-dev/gnustep-gui-0.24.1 && ./configure && make && make install
RUN cd /root/gnustep-dev/gnustep-back-0.24.1 && ./configure && make && make install

RUN apt-get -y install libdispatch-dev

ENV HOME /mal

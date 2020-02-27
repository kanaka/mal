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

# Guile
RUN apt-get -y install libunistring-dev libgc-dev autoconf libtool flex gettext texinfo libgmp-dev
RUN apt-get -y install git pkg-config libffi-dev
# TODO: remove /tmp/guile in same command
RUN git clone git://git.sv.gnu.org/guile.git /tmp/guile \
    && cd /tmp/guile && ./autogen.sh && ./configure && make && make install
RUN ldconfig
# TODO: move this up with other deps
RUN apt-get -y install libpcre3 libpcre3-dev


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

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN DEBIAN_FRONTEND=noninteractive apt-get -y install \
    ca-certificates curl gcc g++ libasound2-dev \
    libglu1-mesa-dev mesa-common-dev patch unzip wget \
    xz-utils libncurses-dev

# cbmbasic
# Remove duplicate RAM (https://github.com/mist64/cbmbasic/commit/352a313313dd0a15a47288c8f8031b54ac8c92a2).
RUN cd /tmp && \
    curl -L https://github.com/kanaka/cbmbasic/archive/master.zip -o cbmbasic.zip && \
    unzip cbmbasic.zip && \
    cd cbmbasic-master && \
    sed -i '/unsigned char RAM.65536.;/d' runtime.c && \
    make && \
    mv cbmbasic /usr/local/bin && \
    cd .. && \
    rm -r cbmbasic*

# qbasic (using freebasic: `fbc -lang qb`)
RUN cd /opt && \
    curl -L https://sourceforge.net/projects/fbc/files/FreeBASIC-1.10.1/Binaries-Linux/FreeBASIC-1.10.1-ubuntu-22.04-x86_64.tar.xz | tar xvJf - && \
    ln -sf /opt/FreeBASIC-1.10.1-ubuntu-22.04-x86_64/bin/fbc /usr/local/bin/fbc


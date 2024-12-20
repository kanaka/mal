FROM ubuntu:24.04 AS base
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
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN apt-get -y install libpcre3-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Compile the io interpreter
##########################################################

FROM base AS builder

RUN apt-get -y install git cmake gcc

RUN cd /tmp \
    && git clone --recursive -q --depth=1 https://github.com/IoLanguage/io.git \
    && cd /tmp/io \
    && mkdir build && cd build \
    && cmake -DCMAKE_BUILD_TYPE=release .. && make && make install

# Force eerie (Io package manager) to install itself and the packages in /opt/.eerie
ENV HOME=/opt

RUN cd /tmp/io/eerie \
    && mkdir -p /opt \
    && . ./install_unix.sh --notouch \
    && eerie install https://github.com/IoLanguage/Range.git \
    && eerie install https://github.com/IoLanguage/ReadLine.git \
    && eerie install https://github.com/IoLanguage/Regex.git

##########################################################
# Specific implementation requirements
##########################################################

FROM base AS io

COPY --from=builder /usr/local/lib/ /usr/lib/
COPY --from=builder /usr/local/bin/ /usr/bin/
COPY --from=builder /opt/.eerie/ /opt/.eerie/

ENV HOME=/mal

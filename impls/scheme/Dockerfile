FROM ubuntu:22.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3 python-is-python3

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# Dev tools
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install gcc g++ bison flex groff make cmake pkg-config git

# Prepackaged Scheme implementations
RUN apt-get -y install gauche chicken-bin guile-3.0
RUN chicken-install r7rs

# Chibi
RUN cd /tmp && curl -Lo chibi-0.10.tar.gz https://github.com/ashinn/chibi-scheme/archive/0.10.tar.gz \
    && tar xvzf chibi-0.10.tar.gz && cd chibi-scheme-0.10 \
    && make && make install && rm -rf /tmp/chibi-*

# Kawa
RUN apt-get -y install openjdk-8-jdk-headless
RUN cd /tmp && curl -O http://ftp.gnu.org/pub/gnu/kawa/kawa-3.1.1.tar.gz \
    && tar xvzf kawa-3.1.1.tar.gz && cd kawa-3.1.1 \
    && ./configure && make && make install && rm -rf /tmp/kawa-3.1.1*

# Sagittarius
RUN apt-get -y install libgc-dev zlib1g-dev libffi-dev libssl-dev
RUN cd /tmp && curl -LO https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/sagittarius-0.9.7.tar.gz \
    && tar xvzf sagittarius-0.9.7.tar.gz && cd sagittarius-0.9.7 \
    && cmake . && make && make install && rm -rf /tmp/sagittarius-0.9.7*

# Cyclone
RUN apt-get -y install libck-dev libtommath-dev
RUN cd /tmp && git clone https://github.com/justinethier/cyclone-bootstrap \
    && cd cyclone-bootstrap \
    && make && make install && rm -rf /tmp/cyclone-bootstrap

# Foment
RUN cd /tmp && git clone https://github.com/leftmike/foment \
    && cd foment/unix && make && cp release/foment /usr/bin/foment \
    && rm -rf /tmp/foment

ENV HOME /mal

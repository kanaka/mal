FROM ubuntu:xenial
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

# Prepackaged Scheme implementations
RUN apt-get -y install gauche chicken-bin

# Chibi
RUN apt-get -y install bison gcc g++ flex
RUN cd /tmp && curl -Lo chibi-0.7.3.tar.gz https://github.com/ashinn/chibi-scheme/archive/0.7.3.tar.gz \
    && tar xvzf chibi-0.7.3.tar.gz && cd chibi-scheme-0.7.3 \
    && make && make install && rm -rf /tmp/chibi-*

# Kawa
RUN apt-get -y install openjdk-8-jdk-headless groff
RUN cd /tmp && curl -O http://ftp.gnu.org/pub/gnu/kawa/kawa-2.4.tar.gz \
    && tar xvzf kawa-2.4.tar.gz && cd kawa-2.4 \
    && ./configure && make && make install && rm -rf /tmp/kawa-2.4*

# Sagittarius
RUN apt-get -y install cmake libgc-dev zlib1g-dev libffi-dev
RUN cd /tmp && curl -LO https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/sagittarius-0.8.3.tar.gz \
    && tar xvzf sagittarius-0.8.3.tar.gz && cd sagittarius-0.8.3 \
    && cmake . && make && make install && rm -rf /tmp/sagittarius-0.8.3*

# Cyclone
RUN apt-get -y install git libtommath-dev
RUN cd /tmp && curl -O http://concurrencykit.org/releases/ck-0.6.0.tar.gz \
    && tar xvzf ck-0.6.0.tar.gz && cd ck-0.6.0 && ./configure PREFIX=/usr \
    && make all && make install && ldconfig && rm -rf /tmp/ck-0.6.0*
RUN cd /tmp && git clone https://github.com/justinethier/cyclone-bootstrap \
    && cd cyclone-bootstrap && make CFLAGS="-O2 -fPIC -rdynamic -Wall -Iinclude -L." \
    && make install && rm -rf /tmp/cyclone-bootstrap

# Foment
RUN cd /tmp && git clone https://github.com/leftmike/foment \
    && cd foment/unix && make && cp release/foment /usr/bin/foment \
    && rm -rf /tmp/foment

ENV HOME /mal

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

# Erlang R17 (so I can use maps)
RUN apt-get -y install build-essential libncurses5-dev libssl-dev
RUN cd /tmp && curl -O http://www.erlang.org/download/otp_src_17.5.tar.gz \
    && tar -C /tmp -zxf /tmp/otp_src_17.5.tar.gz \
    && cd /tmp/otp_src_17.5 && ./configure && make && make install \
    && rm -rf /tmp/otp_src_17.5 /tmp/otp_src_17.5.tar.gz
# Rebar for building the Erlang implementation
RUN apt-get -y install git sudo
RUN cd /tmp/ && git clone -q https://github.com/rebar/rebar.git \
    && cd /tmp/rebar && ./bootstrap && cp rebar /usr/local/bin \
    && rm -rf /tmp/rebar


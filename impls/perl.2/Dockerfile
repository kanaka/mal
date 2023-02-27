FROM ubuntu:jammy
MAINTAINER Ingy döt Net <ingy@ingy.net>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN ln -s python3 /usr/bin/python

RUN apt-get -y install \
        build-essential \
        cpanminus \
        libperl-dev \
        libreadline-dev \
        perl \
 && true

RUN cpanm -n \
        Term::ReadLine \
        Term::ReadLine::Gnu \
        Tie::IxHash \
        XXX \
 && true

ENV PERL5LIB=/mal/impls/perl.2/lib

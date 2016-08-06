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

# Perl6 build deps
RUN apt-get -y install libfile-copy-recursive-perl build-essential git

RUN curl -O http://rakudo.org/downloads/star/rakudo-star-2016.07.tar.gz && \
    tar xzf rakudo-star-2016.07.tar.gz && \
    cd rakudo-star-2016.07 && \
    perl Configure.pl --prefix=/usr --gen-moar --gen-nqp --backends=moar && \
    make && \
    make install && \
    cd .. && \
    rm -rf rakudo-star-2016.07*

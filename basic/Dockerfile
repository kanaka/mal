FROM ubuntu:wily
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

# cbmbasic
RUN apt-get install -y gcc unzip patch
RUN cd /tmp && \
    curl -L https://github.com/kanaka/cbmbasic/archive/master.zip -o cbmbasic.zip && \
    unzip cbmbasic.zip && \
    cd cbmbasic-master && \
    make && \
    cp cbmbasic /usr/bin/cbmbasic && \
    cd .. && \
    rm -r cbmbasic*


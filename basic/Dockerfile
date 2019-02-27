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

RUN apt-get install -y g++ mesa-common-dev libglu1-mesa-dev libasound2-dev wget
RUN cd /tmp && \
    curl -L http://www.qb64.net/release/official/2017_02_09__02_14_38-1.1-20170120.51/linux/qb64-1.1-20170120.51-lnx.tar.gz | tar xzf - && \
    cd qb64 && \
    find . -name '*.sh' -exec sed -i "s/\r//g" {} \; && \
    env EUID=1 ./setup_lnx.sh && \
    mkdir -p /usr/share/qb64 && \
    cp -a qb64 internal LICENSE programs source /usr/share/qb64/ && \
    echo '#!/bin/sh\ncd /usr/share/qb64\n./qb64 "${@}"' > /usr/bin/qb64 && \
    chmod +x /usr/bin/qb64



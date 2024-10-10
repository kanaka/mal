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
    ca-certificates wget

RUN wget -O- \
    https://github.com/janet-lang/janet/releases/download/v1.36.0/janet-v1.36.0-linux-x64.tar.gz \
    | tar -xzC/opt
RUN ln -sf /opt/janet-v1.36.0-linux/bin/janet /usr/local/bin/janet

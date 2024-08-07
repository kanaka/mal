FROM ubuntu:jammy
MAINTAINER Joel Martin <github@martintribe.org>

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

# fix ubuntu
RUN ln -sf bash /bin/sh

# install other tools
RUN apt-get -y install git gcc

# install v
RUN git clone https://github.com/vlang/v /v
RUN cd /v && make && ./v symlink

# install v modules
RUN v -v install edam.greadline
RUN mv /root/.vmodules /

# fix permissions
RUN find /v -type d -print0 | xargs -0 -- chmod a+x
RUN chmod -R a+r /v
RUN find /.vmodules -type d -print0 | xargs -0 -- chmod a+x
RUN chmod -R a+r /.vmodules

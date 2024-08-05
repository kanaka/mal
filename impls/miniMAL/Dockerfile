FROM ubuntu:24.04
MAINTAINER Joel Martin <github@martintribe.org>
LABEL org.opencontainers.image.source=https://github.com/kanaka/mal
LABEL org.opencontainers.image.description="mal test container: miniMAL"

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -sf /usr/bin/python3 /usr/bin/python

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# For building node modules
RUN apt-get -y install g++

RUN apt-get -y install nodejs npm

ENV NPM_CONFIG_CACHE /mal/.npm

# install miniMAL
RUN npm install -g minimal-lisp@1.2.2

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
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

# For building node modules
RUN apt-get -y install g++ libreadline-dev nodejs npm

ENV NPM_CONFIG_CACHE /mal/.npm

# install miniMAL itself
RUN npm install -g minimal-lisp@1.2.2

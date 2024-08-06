FROM ubuntu:24.04
MAINTAINER Joel Martin <github@martintribe.org>
LABEL org.opencontainers.image.source=https://github.com/kanaka/mal
LABEL org.opencontainers.image.description="mal test container: ruby"

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

RUN apt-get -y install ruby

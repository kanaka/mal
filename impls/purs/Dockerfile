FROM ubuntu:24.04

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

RUN DEBIAN_FRONTEND=noninteractive apt-get -y install g++ libreadline-dev nodej
s npm

# Install purescript and deps
RUN apt-get install -y git libtinfo6
RUN npm install -g --unsafe-perm purescript spago

ENV NPM_CONFIG_CACHE /mal/.npm
ENV HOME /mal

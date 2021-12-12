FROM ubuntu:21.10

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

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 10.x stable
RUN apt-get -y install gnupg
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -

# Install nodejs
RUN apt-get -y install nodejs

# Install purescript and deps
RUN apt-get install -y git libtinfo5
RUN npm install -g --unsafe-perm purescript spago

ENV NPM_CONFIG_CACHE /mal/.npm
ENV HOME /mal
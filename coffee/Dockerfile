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

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 0.12 stable
RUN curl -sL https://deb.nodesource.com/setup_0.12 | bash -

# Install nodejs
RUN apt-get -y install nodejs

# Link common name
RUN ln -sf nodejs /usr/bin/node

ENV NPM_CONFIG_CACHE /mal/.npm

# CoffeeScript specific
RUN npm install -g coffee-script
RUN touch /.coffee_history && chmod go+w /.coffee_history


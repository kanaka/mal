FROM ubuntu:20.04
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

# Java and maven
RUN apt-get -y install default-jdk-headless maven
ENV MAVEN_OPTS -Duser.home=/mal

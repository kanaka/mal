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

# Java and Groovy
RUN apt-get -y install openjdk-7-jdk
#RUN apt-get -y install maven2
#ENV MAVEN_OPTS -Duser.home=/mal
RUN apt-get -y install groovy


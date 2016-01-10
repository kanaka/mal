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

# Java and Zip
RUN apt-get -y install openjdk-7-jdk
RUN apt-get -y install unzip

RUN curl -O -J -L https://github.com/JetBrains/kotlin/releases/download/build-1.0.0-beta-4584/kotlin-compiler-1.0.0-beta-4584.zip

RUN mkdir -p /kotlin-compiler
RUN unzip kotlin-compiler-1.0.0-beta-4584.zip -d /kotlin-compiler

ENV KOTLIN_HOME /kotlin-compiler/kotlinc
ENV PATH $KOTLIN_HOME/bin:$PATH

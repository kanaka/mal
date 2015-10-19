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

# Java and maven
RUN apt-get -y install openjdk-7-jdk
#maven2
#ENV MAVEN_OPTS -Duser.home=/mal

ADD https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein \
    /usr/local/bin/lein
RUN chmod 0755 /usr/local/bin/lein
ENV LEIN_HOME /mal/.lein
ENV LEIN_JVM_OPTS -Duser.home=/mal


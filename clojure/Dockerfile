FROM ubuntu:xenial
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

#
# Clojure (Java and lein)
#

RUN apt-get -y install openjdk-8-jdk

ADD https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein \
    /usr/local/bin/lein
RUN chmod 0755 /usr/local/bin/lein
ENV LEIN_HOME /mal/.lein
ENV LEIN_JVM_OPTS -Duser.home=/mal

#
# ClojureScript (Node and Lumo)
#

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 8.X
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -

# Install nodejs.
RUN apt-get -y install nodejs

## Install ffi and lumo-cljs modules globally
#RUN npm install -g ffi lumo-cljs

ENV HOME=/mal


FROM ubuntu:18.04
MAINTAINER Graeme Lockley <graeme.lockley@gmail.com>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal && chmod 777 /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install unzip vim

ENV DENO_INSTALL=/deno
RUN mkdir -p /deno \
    && chmod 777 /deno \
    && curl -fsSL https://deno.land/x/install/install.sh | sh 

ENV PATH=${DENO_INSTALL}/bin:${PATH} \
    DENO_DIR=${DENO_INSTALL}/.cache/deno

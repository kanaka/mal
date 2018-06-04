FROM ubuntu:bionic
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

# Java and Unzip
RUN apt-get -y install openjdk-8-jdk unzip

# Fantom and JLine
RUN cd /tmp && curl -sfLO https://bitbucket.org/fantom/fan-1.0/downloads/fantom-1.0.70.zip \
    && unzip -q fantom-1.0.70.zip \
    && rm fantom-1.0.70.zip \
    && mv fantom-1.0.70 /opt/fantom \
    && cd /opt/fantom \
    && bash adm/unixsetup \
    && curl -sfL -o /opt/fantom/lib/java/jline.jar https://repo1.maven.org/maven2/jline/jline/2.14.6/jline-2.14.6.jar

ENV PATH /opt/fantom/bin:$PATH
ENV HOME /mal

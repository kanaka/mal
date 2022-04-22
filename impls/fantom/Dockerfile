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
RUN cd /tmp && curl -sfLO https://github.com/fantom-lang/fantom/releases/download/v1.0.75/fantom-1.0.75.zip \
    && unzip -q fantom-1.0.75.zip \
    && rm fantom-1.0.75.zip \
    && mv fantom-1.0.75 /opt/fantom \
    && cd /opt/fantom \
    && bash adm/unixsetup \
    && curl -sfL -o /opt/fantom/lib/java/jline.jar https://repo1.maven.org/maven2/jline/jline/2.14.6/jline-2.14.6.jar \
    && sed -i '/java.options/ s/^\/\/ *\(.*\)$/\1 -Djline.expandevents=false/' /opt/fantom/etc/sys/config.props

ENV PATH /opt/fantom/bin:$PATH
ENV HOME /mal

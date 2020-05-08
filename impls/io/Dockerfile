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

# Zip
RUN apt-get -y install unzip

RUN cd /tmp && curl -O -J -L http://iobin.suspended-chord.info/linux/iobin-linux-x64-deb-current.zip \
    && unzip iobin-linux-x64-deb-current.zip IoLanguage-2013.11.04-Linux-x64.deb \
    && dpkg -i IoLanguage-2013.11.04-Linux-x64.deb \
    && ldconfig \
    && rm -f iobin-linux-x64-deb-current.zip IoLanguage-2013.11.04-Linux-x64.deb

ENV HOME /mal

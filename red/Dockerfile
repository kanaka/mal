FROM ubuntu

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

RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get install -y \
	libc6:i386 \
	libcurl3:i386 \
	wget
RUN wget http://static.red-lang.org/dl/linux/red-064
RUN chmod +x red-064

ENV HOME /mal
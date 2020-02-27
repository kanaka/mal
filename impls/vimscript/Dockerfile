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

# To build the readline plugin
RUN apt-get -y install g++

# Vim 8.0
RUN apt-get -y install bzip2
RUN cd /tmp && curl -O ftp://ftp.vim.org/pub/vim/unix/vim-8.0.tar.bz2 \
    && tar xjf /tmp/vim-8.0.tar.bz2 \
    && cd vim80 && make && make install \
    && cd /tmp && rm -r /tmp/vim-8.0.tar.bz2 /tmp/vim80

ENV HOME /mal

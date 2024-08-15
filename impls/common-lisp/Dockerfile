FROM ubuntu:24.04
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apt-get -y install curl git libreadline-dev libedit-dev

# Install sbcl
RUN apt-get -y install sbcl

# Install cl-asdf (CLISP does not seem to come with it)
RUN apt-get -y install cl-launch cl-asdf cl-ppcre

RUN cd /tmp && \
    git clone https://gitlab.common-lisp.net/xcvb/cl-launch.git && \
    cd cl-launch && \
    make install

# Install wget needed to install quicklisp
RUN apt-get -y install wget

# Install quicklisp
RUN HOME=/ && \
    cd /tmp && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --quit --eval '(quicklisp-quickstart:install)' --eval '(ql-util:without-prompting (ql:add-to-init-file))'

RUN chmod -R a+rwx /quicklisp
RUN chmod a+rwx /.sbclrc

RUN mkdir -p /.cache
RUN chmod -R a+rwx /.cache

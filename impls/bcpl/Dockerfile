FROM ubuntu:18.04
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

RUN apt-get -y install gcc mesa-common-dev libglu1-mesa-dev freeglut3-dev

RUN cd /usr/local && \
    curl -o bcpl.tgz https://www.cl.cam.ac.uk/~mr10/BCPL/bcpl.tgz && \
    tar zxf bcpl.tgz && chmod -R a+rX BCPL && rm bcpl.tgz

ENV PATH /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:\
/usr/local/BCPL/cintcode/bin
ENV BCPLROOT /usr/local/BCPL/cintcode
ENV BCPLPATH /usr/local/BCPL/cintcode/cin
ENV BCPLHDRS /usr/local/BCPL/cintcode/g
ENV BCPLSCRIPTS /usr/local/BCPL/cintcode/s

RUN cd /usr/local/BCPL/cintcode && make clean && make


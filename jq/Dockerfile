FROM ubuntu:xenial
MAINTAINER Ali MohammadPur <devanothertest@gmail.com>

# General test requirements
RUN apt-get -y update
RUN apt-get -y install python make
RUN apt-get -y install curl

RUN mkdir -p /mal
WORKDIR /mal

# test requirements
RUN apt-get -y install jq pv

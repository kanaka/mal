FROM alpine
MAINTAINER Ali MohammadPur <devanothertest@gmail.com>

# General test requirements
RUN apk update
RUN apk add python3 make
RUN apk add curl

RUN rm -f `which python` && ln -s `which python3` /usr/bin/python

RUN mkdir -p /mal
WORKDIR /mal

# test requirements
RUN apk add jq bash pv coreutils

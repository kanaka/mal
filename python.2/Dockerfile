FROM alpine:3.10
MAINTAINER Joel Martin <github@martintribe.org>

RUN apk add --no-cache python3 python2 make readline-dev zip bash
RUN python3 -m ensurepip && pip3 install --no-cache --upgrade pip setuptools wheel Arpeggio==1.9.0
RUN rm -f /usr/bin/python && ln -sf python3 /usr/bin/python

RUN mkdir -p /mal
WORKDIR /mal

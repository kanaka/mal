FROM alpine
MAINTAINER Ali MohammadPur <devanothertest@gmail.com>

# General test requirements
RUN apk update
RUN apk add python make
RUN apk add curl

RUN mkdir -p /mal
WORKDIR /mal

# test requirements
RUN apk add jq bash pv

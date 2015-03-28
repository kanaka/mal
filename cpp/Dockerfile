FROM ubuntu:utopic
MAINTAINER Stephen Thirlwall <sdt@dr.com>

RUN apt-get -y update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        g++-4.9 \
        libreadline-dev \
        make \
        && /bin/true

WORKDIR /mal

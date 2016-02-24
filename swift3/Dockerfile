FROM ubuntu:wily
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

# Swift
RUN apt-get -y install clang-3.6 cmake pkg-config \
    git ninja-build uuid-dev libicu-dev icu-devtools \
    libbsd-dev libedit-dev libxml2-dev libsqlite3-dev \
    swig libpython-dev libncurses5-dev

ENV SWIFT_PREFIX swift-DEVELOPMENT-SNAPSHOT-2016-02-08-a
ENV SWIFT_RELEASE ${SWIFT_PREFIX}-ubuntu15.10

RUN cd /opt && \
    curl -O https://swift.org/builds/development/ubuntu1510/${SWIFT_PREFIX}/${SWIFT_RELEASE}.tar.gz && \
    tar xvzf ${SWIFT_RELEASE}.tar.gz && \
    rm ${SWIFT_RELEASE}.tar.gz

#    tar xvzf ${SWIFT_RELEASE}.tar.gz --directory / --strip-components 1 && \
#RUN find /usr -type f | xargs -ifoo chmod go+r foo && \
#    find /usr -type d | xargs -ifoo chmod go+rx foo

#RUN find /opt/${SWIFT_RELEASE}/ -type f | xargs -ifoo chmod go+r foo && \
#    find /opt/${SWIFT_RELEASE}/ -type d | xargs -ifoo chmod go+rx foo

ENV PATH /opt/${SWIFT_RELEASE}/usr/bin/:$PATH

# TODO: better way to do this? And move up.
RUN ln -sf /usr/lib/llvm-3.6/bin/clang++ /usr/bin/clang++
RUN ln -sf /usr/lib/llvm-3.6/bin/clang /usr/bin/clang


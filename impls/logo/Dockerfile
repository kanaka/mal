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

# Rebuild ucblogo.
# * without X libraries so that the executable starts in text mode.
# * Add the timems function implemented in C

RUN apt -y install autoconf autoconf-archive automake dpkg-dev g++ libncurses-dev

RUN sed -i 's/Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources
RUN apt-get -y update

RUN cd /tmp \
    && apt-get source ucblogo \
    && cd /tmp/ucblogo-* \
    && autoreconf -f -i \
    && ./configure --disable-docs --disable-x11 \
    && echo "extern NODE *ltimems(NODE *);" >> globals.h \
    && echo "NODE *ltimems(NODE *args) { struct timeval tv; gettimeofday(&tv, NULL); return(make_floatnode(((FLONUM)tv.tv_sec) * 1000.0 + (tv.tv_usec / 1000))); }" >> coms.c \
    && sed -i -e 's/^\(.*lthrow.*\)$/\1 {"timems", 0, 0, 0, PREFIX_PRIORITY, ltimems},/' init.c \
    && make \
    && make install \
    && cd /tmp \
    && rm -rf /tmp/ucblogo*

ENV HOME /mal

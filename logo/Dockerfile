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

# Install g++ for any C/C++ based implementations
RUN apt-get -y install g++

# Install UCBLogo 6.0:
# * Fix the makefile to build correctly
# * Tweat GC settings to improve performance (it's still very slow)
# * Add the timems function implemented in C
RUN apt-get -y install libx11-dev \
    && cd /tmp \
    && curl -O -J -L http://www.cs.berkeley.edu/~bh/downloads/ucblogo.tar.gz \
    && tar xf ucblogo.tar.gz \
    && cd /tmp/ucblogo-6.0 \
    && rm -rf csls/CVS \
    && ./configure \
    && sed -i -e 's/svnversion/echo 206/' -e 's/^\s*(cd docs/#\0/' makefile \
    && echo "all: everything" >> makefile \
    && sed -i -e 's/^#define *SEG_SIZE *16000 /#define SEG_SIZE 6400000 /' logo.h \
    && sed -i -e 's/^#define GCMAX 16000$/#define GCMAX 16000000/' mem.c \
    && echo "extern NODE *ltimems(NODE *);" >> globals.h \
    && echo "NODE *ltimems(NODE *args) { struct timeval tv; gettimeofday(&tv, NULL); return(make_floatnode(((FLONUM)tv.tv_sec) * 1000.0 + (tv.tv_usec / 1000))); }" >> coms.c \
    && sed -i -e 's/^\(.*lthrow.*\)$/\1 {"timems", 0, 0, 0, PREFIX_PRIORITY, ltimems},/' init.c \
    && make install \
    && cd /tmp \
    && rm -rf /tmp/ucblogo.tar.gz /tmp/ucblogo-6.0

ENV HOME /mal

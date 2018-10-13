FROM ubuntu:18.04

RUN dpkg --add-architecture i386 && \
    apt-get -y update && \
    apt-get -y install \
        git-core cmake g++ lib32gcc-8-dev \
        libsdl2-dev:i386 libsdl2-image-dev:i386 \
        libedit-dev:i386

# TODO: merge up
RUN apt-get -y install python

RUN git clone https://github.com/WebAssembly/binaryen/ && \
    cd binaryen && \
    cmake . && make && \
    make install

# TODO: merge up
RUN apt-get -y install freeglut3-dev:i386 lib32gcc-7-dev libreadline-dev:i386

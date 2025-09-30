FROM ubuntu:22.04

RUN apt-get update && \
    apt-get install -qqy git clang build-essential python3 python-is-python3
RUN git clone https://github.com/dzaima/CBQN.git && \
    cd CBQN && \
    git reset --hard 88f65850fa6ac28bc50886c5942652f21d5be924 && \
    make && \
    make install && \
    ln -s bqn /usr/local/bin/BQN
RUN mkdir /mal
WORKDIR /mal

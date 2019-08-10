FROM ubuntu:18.04 as base 
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

#
# node
#

# For building node modules
RUN apt-get -y install g++

# Add nodesource apt repo config for 10.x stable
RUN apt-get -y install gnupg
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -

# Install nodejs
RUN apt-get -y install nodejs

ENV NPM_CONFIG_CACHE /mal/.npm

#
# wace build and runtime libs
#
RUN dpkg --add-architecture i386 && \
    apt-get -y update && \
    apt-get -y install \
        lib32gcc-8-dev libsdl2-dev:i386 libsdl2-image-dev:i386 \
        libedit-dev:i386 freeglut3-dev:i386 lib32gcc-7-dev \
        libreadline-dev:i386

#
# binaryen
#
RUN apt-get -y install git-core cmake

RUN git clone https://github.com/WebAssembly/binaryen/ && \
    cd binaryen && \
    cmake . && make && \
    make install && \
    cd .. && \
    rm -r binaryen

###########################################################################
FROM base as build_tools
###########################################################################

#
# clang/LLVM and rust (for building wasmtime)
#
RUN apt-get -y install llvm-3.9-dev libclang-3.9-dev clang-3.9
RUN apt-get -y install curl &&  \
    curl https://sh.rustup.rs -sSf > /tmp/rustup.sh && \
    sh /tmp/rustup.sh -y
ENV PATH $PATH:/root/.cargo/bin

#
# pypy / rpython (for building warpy)
#

# rpython deps
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -y install libffi-dev pkg-config libz-dev \
    libbz2-dev libsqlite3-dev libncurses-dev libexpat1-dev \
    libssl-dev libgdbm-dev tcl-dev

# install pypy, build and install pypy/rpython, remove prior pypy
RUN apt-get -y install software-properties-common && \
    add-apt-repository ppa:pypy && \
    apt-get -y update && \
    apt-get -y install pypy && \
    mkdir -p /opt/pypy && \
    curl -L https://bitbucket.org/pypy/pypy/downloads/pypy2-v6.0.0-src.tar.bz2 \
        | tar -xjf - -C /opt/pypy/ --strip-components=1 && \
    cd /opt/pypy && make && \
    chmod -R ugo+rw /opt/pypy/rpython/_cache && \
    ln -sf /opt/pypy/rpython/bin/rpython /usr/local/bin/rpython && \
    ln -sf /opt/pypy/pypy-c /usr/local/bin/pypy && \
    rm -rf /tmp/usession* && \
    ln -sf /opt/pypy/pypy/goal/pypy-c /usr/local/bin/pypy && \
    apt-get -y autoremove pypy


#
# wasi-sdk (C/C++ -> wasm+wasi)
#
RUN curl -LO https://github.com/CraneStation/wasi-sdk/releases/download/wasi-sdk-5/wasi-sdk_5.0_amd64.deb && \
    dpkg -i wasi-sdk_5.0_amd64.deb && \
    rm wasi-sdk_5.0_amd64.deb

##
## Rust wasm support
##
#RUN rustup default nightly
#RUN rustup target add wasm32-unknown-wasi --toolchain nightly
##RUN cargo +nightly build --target wasm32-unknown-wasi
#
## TODO: Do this when we install rust instead
#RUN mv /root/.cargo /opt/cargo && mv /root/.rustup /opt/rustup
#RUN chmod -R a+r /opt/cargo && chmod -R a+rw /opt/rustup
#ENV CARGO_HOME /opt/cargo
#ENV RUSTUP_HOME /opt/rustup
#ENV PATH $PATH:/opt/cargo/bin

###########################################################################
FROM build_tools as runtimes
###########################################################################

#
# warpy
#
RUN git clone https://github.com/kanaka/warpy/  && \
    cd warpy && \
    make warpy-nojit && \
    cp warpy-nojit /usr/bin/warpy

#
# wac/wace
#
RUN git clone https://github.com/kanaka/wac/ && \
    cd wac  && \
    make USE_SDL= wac wax wace && \
    cp wac wax wace /usr/bin

#
# wasmer
#

RUN curl https://get.wasmer.io -sSfL | sh && \
    cp /root/.wasmer/bin/wasmer /usr/bin/wasmer && \
    cp /root/.wasmer/bin/wapm /usr/bin/wapm

#RUN git clone --recursive https://github.com/wasmerio/wasmer  && \
#    cd wasmer && \
#    cargo build --release && \
#    cp target/release/wasmer /usr/bin/

#
# lucet
#

RUN git clone --recursive https://github.com/fastly/lucet && \
    cd lucet && \
    make install

#
# wasmtime
#

RUN curl -L https://github.com/CraneStation/wasmtime/releases/download/dev/wasmtime-dev-x86_64-linux.tar.xz | tar xvJf - && \
    cp wasmtime-dev-x86_64-linux/wasmtime /usr/bin/wasmtime && \
    cp wasmtime-dev-x86_64-linux/wasm2obj /usr/bin/wasm2obj

#RUN git clone --recursive https://github.com/CraneStation/wasmtime &&  \
#    cd wasmtime && \
#    sed -i 's/c3994bf57b5d2f1f973b0e4e37bc385695aa4ed2/8ea7a983d8b1364e5f62d2adf0e74b3b8db1c9b3/' Cargo.toml && \
#    cargo build --release && \
#    cp target/release/wasmtime /usr/bin/ && \
#    cp target/release/wasm2obj /usr/bin/


###########################################################################
FROM base as wasm
###########################################################################

COPY --from=runtimes /usr/bin/wac /usr/bin/wac
COPY --from=runtimes /usr/bin/wax /usr/bin/wax
COPY --from=runtimes /usr/bin/wace /usr/bin/wace
COPY --from=runtimes /usr/bin/warpy /usr/bin/warpy
COPY --from=runtimes /usr/bin/wasmtime /usr/bin/wasmtime
COPY --from=runtimes /usr/bin/wasm2obj /usr/bin/wasm2obj

RUN mkdir -p /opt/lucet/bin
COPY --from=runtimes /opt/lucet/ /opt/lucet
RUN ln -sf /opt/lucet/bin/lucetc-wasi /usr/bin/lucetc-wasi
RUN ln -sf /opt/lucet/bin/lucet-wasi /usr/bin/lucet-wasi

COPY --from=runtimes /usr/bin/wasmer /usr/bin/wasmer
COPY --from=runtimes /usr/bin/wapm /usr/bin/wapm


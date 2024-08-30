FROM ubuntu:24.04
LABEL org.opencontainers.image.source=https://github.com/kanaka/mal
LABEL org.opencontainers.image.description="mal test container: lean"

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

RUN apt-get -y install curl git-core

# install lean toolchain manager
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y -v

RUN /root/.elan/bin/elan default leanprover/lean4:stable

# non-root users must have access
RUN mv /root/.elan/bin/lake /usr/local/bin/

# lake needs to create $HOME/.elan
ENV HOME=/mal

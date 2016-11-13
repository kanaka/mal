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

# Nothing additional needed for python
RUN apt-get -y install libunwind8 libicu52
#RUN apt-get -y install libunwind8 libicu55

# For dist packaging
RUN curl -L -O https://github.com/PowerShell/PowerShell/releases/download/v6.0.0-alpha.9/powershell_6.0.0-alpha.9-1ubuntu1.14.04.1_amd64.deb && \
    dpkg -i powershell_6.0.0-alpha.9-1ubuntu1.14.04.1_amd64.deb && \
    rm powershell_6.0.0-alpha.9-1ubuntu1.14.04.1_amd64.deb
#RUN curl -L -O https://github.com/PowerShell/PowerShell/releases/download/v6.0.0-alpha.9/powershell_6.0.0-alpha.9-1ubuntu1.16.04.1_amd64.deb && \
#    dpkg -i powershell_6.0.0-alpha.9-1ubuntu1.16.04.1_amd64.deb && \
#    rm powershell_6.0.0-alpha.9-1ubuntu1.16.04.1_amd64.deb

ENV HOME=/mal

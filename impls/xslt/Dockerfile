FROM ubuntu:bionic
MAINTAINER Joel Martin <github@martintribe.org>

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python

# Some typical implementation and test requirements
RUN apt-get -y install curl libreadline-dev libedit-dev libpcre3-dev

RUN mkdir -p /mal
WORKDIR /mal

#########################################################
# Specific implementation requirements
#########################################################

RUN apt-get -y install python3.8 openjdk-8-jre
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.8 10

RUN curl https://repo1.maven.org/maven2/net/sf/saxon/Saxon-HE/9.9.1-6/Saxon-HE-9.9.1-6.jar -O

RUN mv Saxon-HE-9.9.1-6.jar /bin/Saxon-HE-9.9.1-6.jar

RUN echo -n "#!/bin/sh\njava -Xmx2G -cp /bin/Saxon-HE-9.9.1-6.jar net.sf.saxon.Transform \"\$@\"" > /usr/bin/saxon && chmod +x /usr/bin/saxon



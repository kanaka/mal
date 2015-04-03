FROM ubuntu:utopic
MAINTAINER Joel Martin <github@martintribe.org>

ENV DEBIAN_FRONTEND noninteractive

RUN echo "deb http://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list
RUN apt-get -y update

#
# General dependencies
#
VOLUME /mal

RUN apt-get -y install make wget curl git

# Deps for compiled languages (C, Go, Rust, Nim, etc)
RUN apt-get -y install gcc pkg-config 

# Deps for Java-based languages (Clojure, Scala, Java)
RUN apt-get -y install openjdk-7-jdk
ENV MAVEN_OPTS -Duser.home=/mal

# Deps for Mono-based languages (C#, VB.Net)
RUN apt-get -y install mono-runtime mono-mcs mono-vbnc

# Deps for node.js languages (JavaScript, CoffeeScript, miniMAL, etc)
RUN apt-get -y install nodejs npm
RUN ln -sf nodejs /usr/bin/node


#
# Implementation specific installs
#

# Bash
RUN apt-get -y install bash

# C
RUN apt-get -y install libglib2.0 libglib2.0-dev
RUN apt-get -y install libffi-dev libreadline-dev libedit2 libedit-dev

# C++
RUN apt-get -y install g++-4.9 libreadline-dev

# Clojure
ADD https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein \
    /usr/local/bin/lein
RUN sudo chmod 0755 /usr/local/bin/lein
ENV LEIN_HOME /mal/.lein
ENV LEIN_JVM_OPTS -Duser.home=/mal

# CoffeeScript
RUN npm install -g coffee-script
RUN touch /.coffee_history && chmod go+w /.coffee_history

# C#
RUN apt-get -y install mono-mcs

# Forth
RUN apt-get -y install gforth

# Go
RUN apt-get -y install golang

# Haskell
RUN apt-get -y install ghc haskell-platform libghc-readline-dev libghc-editline-dev

# Java
RUN apt-get -y install maven2

# JavaScript
# Already satisfied above

# Lua
RUN apt-get -y install lua5.1 lua-rex-pcre luarocks
RUN luarocks install linenoise

# Mal
# N/A: self-hosted on other language implementations

# GNU Make
# Already satisfied as a based dependency for testing

# miniMAL
RUN npm install -g minimal-lisp

# Nim
RUN git clone -b devel git://github.com/Araq/Nim.git /tmp/Nim \
    && cd /tmp/Nim && git clone -b devel --depth 1 git://github.com/nim-lang/csources \
    && cd /tmp/Nim/csources && sh build.sh \
    && cd /tmp/Nim && bin/nim c koch \
    && ./koch boot -d:release \
    && ./koch install /usr/local/bin \
    && rm -r /tmp/Nim

# OCaml
RUN apt-get -y install ocaml-batteries-included

# perl
RUN apt-get -y install perl

# PHP
RUN apt-get -y install php5-cli

# PostScript/ghostscript
RUN apt-get -y install ghostscript

# python
RUN apt-get -y install python

# R
RUN apt-get -y install r-base-core

# Racket
RUN apt-get -y install racket

# Ruby
RUN apt-get -y install ruby

# Rust
RUN curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh

# Scala
RUN apt-get -y --force-yes install sbt
RUN apt-get -y install scala
ENV SBT_OPTS -Duser.home=/mal

# VB.Net
RUN apt-get -y install mono-vbnc

# TODO: move this up
# Julia
RUN apt-add-repository -y ppa:staticfloat/juliareleases
RUN apt-get -y update
RUN apt-get -y install julia


# MATLAB is proprietary/licensed. Maybe someday with Octave.
# Swift is XCode/OS X only
ENV SKIP_IMPLS matlab swift

ENV DEBIAN_FRONTEND newt
ENV HOME /

WORKDIR /mal

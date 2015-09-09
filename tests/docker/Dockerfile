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

# GNU awk
RUN apt-get -y install gawk

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

# Elixir
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb \
    && dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update
RUN apt-get -y install elixir

# Erlang R17 (so I can use maps)
RUN apt-get -y install build-essential libncurses5-dev libssl-dev
RUN cd /tmp && wget http://www.erlang.org/download/otp_src_17.5.tar.gz \
    && tar -C /tmp -zxf /tmp/otp_src_17.5.tar.gz \
    && cd /tmp/otp_src_17.5 && ./configure && make && make install \
    && rm -rf /tmp/otp_src_17.5 /tmp/otp_src_17.5.tar.gz
# Rebar for building the Erlang implementation
RUN cd /tmp/ && git clone -q https://github.com/rebar/rebar.git \
    && cd /tmp/rebar && ./bootstrap && cp rebar /usr/local/bin \
    && rm -rf /tmp/rebar

# Forth
RUN apt-get -y install gforth

# Go
RUN apt-get -y install golang

# Guile
RUN apt-get -y install libunistring-dev libgc-dev autoconf libtool flex gettext texinfo libgmp-dev
RUN git clone git://git.sv.gnu.org/guile.git /tmp/guile \
    && cd /tmp/guile && ./autogen.sh && ./configure && make && make install

# Haskell
RUN apt-get -y install ghc haskell-platform libghc-readline-dev libghc-editline-dev

# Java
RUN apt-get -y install maven2

# JavaScript
# Already satisfied above

# Julia
RUN apt-get -y install software-properties-common
RUN apt-add-repository -y ppa:staticfloat/juliareleases
RUN apt-get -y update
RUN apt-get -y install julia

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
RUN cd /tmp && wget http://nim-lang.org/download/nim-0.11.0.tar.xz \
    && tar xvJf /tmp/nim-0.11.0.tar.xz && cd nim-0.11.0 \
    && make && sh install.sh /usr/local/bin \
    && rm -r /tmp/nim-0.11.0

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

# TODO: move up
# Factor
RUN apt-get -y install libgtkglext1
RUN cd /usr/lib/x86_64-linux-gnu/ \
    && wget http://downloads.factorcode.org/releases/0.97/factor-linux-x86-64-0.97.tar.gz \
    && tar xvzf factor-linux-x86-64-0.97.tar.gz \
    && ln -sf /usr/lib/x86_64-linux-gnu/factor/factor /usr/bin/factor \
    && rm factor-linux-x86-64-0.97.tar.gz

# MATLAB is proprietary/licensed. Maybe someday with Octave.
# Swift is XCode/OS X only
ENV SKIP_IMPLS matlab swift

ENV DEBIAN_FRONTEND newt
ENV HOME /

WORKDIR /mal

# Compilation notes

## Mac OSX

This C++ implementation was developed on Mac OS X Yosemite, and uses the
stock g++ compiler.

The only other requirement is GNU Readline, which I got from homebrew.

    brew install readline

You may need to edit the READLINE path in the Makefile.

## Ubuntu 14.10/15.04

This should compile on Ubuntu 14.10 and 15.04 with the following packages

    apt-get install clang-3.5 libreadline-dev make

## Docker

For everyone else, there is a Dockerfile and associated docker.sh script which
can be used to make and run this implementation.

    * build the docker image

        ./docker build

    * make the MAL binaries:

        ./docker make

    * run one of the implemenations:

        ./docker run ./stepA_mal

    * open a shell inside the docker container:

        ./docker run

FROM archlinux/base
MAINTAINER Ali MohammadPur <devanothertest@gmail.com>

# General test requirements
RUN pacman -Sy
RUN pacman -S --noconfirm python make
RUN pacman -S --noconfirm curl

RUN mkdir -p /mal
WORKDIR /mal

# test requirements
RUN pacman -S --noconfirm jq pv coreutils


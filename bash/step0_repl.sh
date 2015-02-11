#!/usr/bin/env bash

READ () {
    read -u 0 -e -p "user> " r
}

EVAL () {
    r=
    eval "${1}"
}

PRINT () {
    r="${1}"
}

while true; do
    READ
    EVAL "${r}"
    PRINT "${r}"
    echo "${r}"
done

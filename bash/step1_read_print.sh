#!/bin/bash

source $(dirname $0)/reader.sh
source $(dirname $0)/printer.sh

# read
READ () {
    [ "${1}" ] && r="${1}" || READLINE
    READ_STR "${r}"
}

# eval
EVAL () {
    local ast="${1}"
    local env="${2}"
    r=
    [[ "${__ERROR}" ]] && return 1
    r="${ast}"
}

# print
PRINT () {
    if [[ "${__ERROR}" ]]; then
        _pr_str "${__ERROR}" yes
        r="Error: ${r}"
        __ERROR=
    else
        _pr_str "${1}" yes
    fi
}

# repl
REP () {
    READ "${1}" || return 1
    EVAL "${r}"
    PRINT "${r}"
}

# repl loop
while true; do
    READLINE "user> " || exit "$?"
    [[ "${r}" ]] && REP "${r}" && echo "${r}"
done

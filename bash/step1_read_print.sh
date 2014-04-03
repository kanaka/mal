#!/bin/bash

INTERACTIVE=${INTERACTIVE-yes}

source $(dirname $0)/reader.sh
source $(dirname $0)/printer.sh

# READ: read and parse input
READ () {
    [ "${1}" ] && r="${1}" || READLINE
    READ_STR "${r}"
}

# EVAL: just return the input
EVAL () {
    local ast="${1}"
    local env="${2}"
    r=
    [[ "${__ERROR}" ]] && return 1
    r="${ast}"
}

# PRINT:
PRINT () {
    if [[ "${__ERROR}" ]]; then
        _pr_str "${__ERROR}" yes
        r="Error: ${r}"
        __ERROR=
    else
        _pr_str "${1}" yes
    fi
}

# REPL: read, eval, print, loop
REP () {
    READ "${1}" || return 1
    EVAL "${r}"
    PRINT "${r}"
}

if [[ -n "${INTERACTIVE}" ]]; then
    while true; do
        READLINE "user> " || exit "$?"
        [[ "${r}" ]] && REP "${r}" && echo "${r}"
    done
fi

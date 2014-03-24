#!/bin/bash

INTERACTIVE=${INTERACTIVE-yes}

source $(dirname $0)/reader.sh

# READ: read and parse input
READ () {
    READLINE
    READ_STR "${r}"
}

EVAL_AST () {
    local ast="${1}" env="${2}"
    #_pr_str "${ast}"; echo "EVAL_AST '${ast}:${r} / ${env}'"
    _obj_type "${ast}"; local ot="${r}"
    case "${ot}" in
    symbol)
        local val="${ANON["${ast}"]}"
        eval r="\${${env}["${val}"]}"
        [ "${r}" ] || _error "'${val}' not found" ;;
    list)
        _map_with_type list EVAL "${ast}" "${env}" ;;
    vector)
        _map_with_type vector EVAL "${ast}" "${env}" ;;
    hash_map)
        local res="" val="" hm="${ANON["${ast}"]}"
        hash_map; local new_hm="${r}"
        eval local keys="\${!${hm}[@]}"
        for key in ${keys}; do
            eval val="\${${hm}[\"${key}\"]}"
            EVAL "${val}" "${env}"
            assoc! "${new_hm}" "${key}" "${r}"
        done
        r="${new_hm}" ;;
    *)
        r="${ast}" ;;
    esac
}

# EVAL: evaluate the parameter
EVAL () {
    local ast="${1}" env="${2}"
    r=
    [[ "${__ERROR}" ]] && return 1
    #_pr_str "${ast}"; echo "EVAL '${r} / ${env}'"
    _obj_type "${ast}"; local ot="${r}"
    if [[ "${ot}" != "list" ]]; then
        EVAL_AST "${ast}" "${env}"
        return
    fi

    # apply list
    EVAL_AST "${ast}" "${env}"
    [[ "${__ERROR}" ]] && return 1
    local el="${r}"
    first "${el}"; local f="${r}"
    rest "${el}"; local args="${ANON["${r}"]}"
    #echo "invoke: ${f} ${args}"
    eval ${f} ${args}
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
declare -A REPL_ENV
REP () {
    READ_STR "${1}"
    EVAL "${r}" REPL_ENV
    PRINT "${r}"
}

REPL_ENV["+"]=num_plus
REPL_ENV["-"]=num_minus
REPL_ENV["__STAR__"]=num_multiply
REPL_ENV["/"]=num_divide

if [[ -n "${INTERACTIVE}" ]]; then
    while true; do
        READLINE "user> " || exit "$?"
        [[ "${r}" ]] && REP "${r}" && echo "${r}"
    done
fi

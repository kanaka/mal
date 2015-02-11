#!/usr/bin/env bash

source $(dirname $0)/reader.sh
source $(dirname $0)/printer.sh

# read
READ () {
    [ "${1}" ] && r="${1}" || READLINE
    READ_STR "${r}"
}

# eval
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
        _map_with_type _list EVAL "${ast}" "${env}" ;;
    vector)
        _map_with_type _vector EVAL "${ast}" "${env}" ;;
    hash_map)
        local res="" val="" hm="${ANON["${ast}"]}"
        _hash_map; local new_hm="${r}"
        eval local keys="\${!${hm}[@]}"
        for key in ${keys}; do
            eval val="\${${hm}[\"${key}\"]}"
            EVAL "${val}" "${env}"
            _assoc! "${new_hm}" "${key}" "${r}"
        done
        r="${new_hm}" ;;
    *)
        r="${ast}" ;;
    esac
}

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
    _first "${el}"; local f="${r}"
    _rest "${el}"; local args="${ANON["${r}"]}"
    #echo "invoke: ${f} ${args}"
    eval ${f} ${args}
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
declare -A REPL_ENV
REP () {
    r=
    READ "${1}"
    EVAL "${r}" REPL_ENV
    PRINT "${r}"
}

plus     () { r=$(( ${ANON["${1}"]} + ${ANON["${2}"]} )); _number "${r}"; }
minus    () { r=$(( ${ANON["${1}"]} - ${ANON["${2}"]} )); _number "${r}"; }
multiply () { r=$(( ${ANON["${1}"]} * ${ANON["${2}"]} )); _number "${r}"; }
divide   () { r=$(( ${ANON["${1}"]} / ${ANON["${2}"]} )); _number "${r}"; }

REPL_ENV["+"]=plus
REPL_ENV["-"]=minus
REPL_ENV["__STAR__"]=multiply
REPL_ENV["/"]=divide

# repl loop
while true; do
    READLINE "user> " || exit "$?"
    [[ "${r}" ]] && REP "${r}" && echo "${r}"
done

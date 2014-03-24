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
        ENV_GET "${env}" "${val}"
        return ;;
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
    while true; do
    r=
    [[ "${__ERROR}" ]] && return 1
    #_pr_str "${ast}"; echo "EVAL '${r} / ${env}'"
    _obj_type "${ast}"; local ot="${r}"
    if [[ "${ot}" != "list" ]]; then
        EVAL_AST "${ast}" "${env}"
        return
    fi

    # apply list
    _nth "${ast}" 0; local a0="${r}"
    _nth "${ast}" 1; local a1="${r}"
    _nth "${ast}" 2; local a2="${r}"
    case "${ANON["${a0}"]}" in
        def!) local k="${ANON["${a1}"]}"
              #echo "def! ${k} to ${a2} in ${env}"
              EVAL "${a2}" "${env}"
              ENV_SET "${env}" "${k}" "${r}"
              return ;;
        let*) ENV "${env}"; local let_env="${r}"
              local let_pairs=(${ANON["${a1}"]})
              local idx=0
              #echo "let: [${let_pairs[*]}] for ${a2}"
              while [[ "${let_pairs["${idx}"]}" ]]; do
                  EVAL "${let_pairs[$(( idx + 1))]}" "${let_env}"
                  ENV_SET "${let_env}" "${ANON["${let_pairs[${idx}]}"]}" "${r}"
                  idx=$(( idx + 2))
              done
              EVAL "${a2}" "${let_env}"
              return ;;
        do)   _count "${ast}"
              _slice "${ast}" 1 $(( ${r} - 2 ))
              EVAL_AST "${r}" "${env}"
              [[ "${__ERROR}" ]] && r= && return 1
              last "${ast}"
              ast="${r}"
              # Continue loop
              ;;
        if)   EVAL "${a1}" "${env}"
              if [[ "${r}" == "${__false}" || "${r}" == "${__nil}" ]]; then
                  # eval false form
                  _nth "${ast}" 3; local a3="${r}"
                  if [[ "${a3}" ]]; then
                      ast="${a3}"
                  else
                      r="${__nil}"
                      return
                  fi
              else
                  # eval true condition
                  ast="${a2}"
              fi
              # Continue loop
              ;;
        fn*)  new_function "ENV \"${env}\" \"${a1}\" \"\${@}\"; \
                            EVAL \"${a2}\" \"\${r}\"" \
                           "${a2}" "${env}" "${a1}"
              return ;;
        *)    EVAL_AST "${ast}" "${env}"
              [[ "${__ERROR}" ]] && r= && return 1
              local el="${r}"
              first "${el}"; local f="${ANON["${r}"]}"
              rest "${el}"; local args="${ANON["${r}"]}"
              #echo "invoke: [${f}] ${args}"
              if [[ "${f//@/ }" != "${f}" ]]; then
                  set -- ${f//@/ }
                  ast="${2}"
                  ENV "${3}" "${4}" ${args}
                  env="${r}"
              else
                  eval ${f%%@*} ${args}
                  return
              fi
              # Continue loop
              ;;
    esac
    done
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
ENV; REPL_ENV="${r}"
REP () {
    r=
    READ_STR "${1}"
    EVAL "${r}" ${REPL_ENV}
    PRINT "${r}"
}

_fref () { new_function "${2} \"\${@}\""; ENV_SET "${REPL_ENV}" "${1}" "${r}"; }

# Import types functions
for n in "${!types_ns[@]}"; do _fref "${n}" "${types_ns["${n}"]}"; done

read_string () { READ_STR "${ANON["${1}"]}"; }
_fref "read-string" read_string
_eval () { EVAL "${1}" "${REPL_ENV}"; }
_fref "eval" _eval
slurp () { string "$(cat "${ANON["${1}"]}")"; }
_fref "slurp" slurp
slurp_do () { string "(do $(cat "${ANON["${1}"]}"))"; }
_fref "slurp-do" slurp_do

# Defined using the language itself
REP "(def! not (fn* (a) (if a false true)))"
REP "(def! load-file (fn* (f) (eval (read-string (slurp-do f)))))"

if [[ "${1}" ]]; then
    echo "${@}"
    REP "(load-file \"${1}\")" && echo "${r}"
elif [[ -n "${INTERACTIVE}" ]]; then
    while true; do
        READLINE "user> " || exit "$?"
        [[ "${r}" ]] && REP "${r}" && echo "${r}"
    done
fi

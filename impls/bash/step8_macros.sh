#!/usr/bin/env bash

source $(dirname $0)/reader.sh
source $(dirname $0)/printer.sh
source $(dirname $0)/env.sh
source $(dirname $0)/core.sh

# read
READ () {
    [ "${1}" ] && r="${1}" || READLINE
    READ_STR "${r}"
}

# eval
starts_with () {
    _list? "$1" && _first "$1" && _symbol? "$r" && [ "${ANON[$r]}" = "$2" ]
}

QUASIQUOTE () {
    _obj_type "$1"
    case "$r" in
        list)
            if starts_with "$1" unquote; then
                _nth "$1" 1
            else
                qqIter "$1"
            fi ;;
        vector)
            _symbol vec;  local a="$r"
            qqIter "$1"
            _list "$a" "$r" ;;
        symbol|hash_map)
            _symbol quote
            _list "$r" "$1" ;;
        *)
            r="$1" ;;
    esac
}

qqIter () {
    if _empty? "$1"; then
        _list
    else
        _nth "${1}" 0;        local a0="$r"
        if starts_with "$a0" splice-unquote; then
            _symbol concat;   local a="$r"
            _nth "$a0" 1;     local b="$r"
        else
            _symbol cons;     local a="$r"
            QUASIQUOTE "$a0"; local b="$r"
        fi
        _rest "$1"
        qqIter "$r"
        _list "$a" "$b" "$r"
    fi
}

IS_MACRO_CALL () {
    if ! _list? "${1}"; then return 1; fi
    _nth "${1}" 0; local a0="${r}"
    if _symbol? "${a0}"; then
        ENV_FIND "${2}" "${a0}"
        if [[ "${r}" ]]; then
            ENV_GET "${2}" "${a0}"
            [ "${ANON["${r}_ismacro_"]}" ]
            return $?
        fi
    fi
    return 1
}

MACROEXPAND () {
    local ast="${1}" env="${2}"
    while IS_MACRO_CALL "${ast}" "${env}"; do
        _nth "${ast}" 0; local a0="${r}"
        ENV_GET "${env}" "${a0}"; local mac="${ANON["${r}"]}"
        _rest "${ast}"
        ${mac%%@*} ${ANON["${r}"]}
        ast="${r}"
    done
    r="${ast}"
}


EVAL_AST () {
    local ast="${1}" env="${2}"
    #_pr_str "${ast}"; echo "EVAL_AST '${ast}:${r} / ${env}'"
    _obj_type "${ast}"; local ot="${r}"
    case "${ot}" in
    symbol)
        ENV_GET "${env}" "${ast}"
        return ;;
    list)
        _map_with_type _list EVAL "${ast}" "${env}" ;;
    vector)
        _map_with_type _vector EVAL "${ast}" "${env}" ;;
    hash_map)
        local res="" key= val="" hm="${ANON["${ast}"]}"
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
    while true; do
    r=
    [[ "${__ERROR}" ]] && return 1
    #_pr_str "${ast}"; echo "EVAL '${r} / ${env}'"
    if ! _list? "${ast}"; then
        EVAL_AST "${ast}" "${env}"
        return
    fi

    # apply list
    MACROEXPAND "${ast}" "${env}"
    ast="${r}"
    if ! _list? "${ast}"; then
        EVAL_AST "${ast}" "${env}"
        return
    fi
    _empty? "${ast}" && r="${ast}" && return

    _nth "${ast}" 0; local a0="${r}"
    _nth "${ast}" 1; local a1="${r}"
    _nth "${ast}" 2; local a2="${r}"
    case "${ANON["${a0}"]}" in
        def!) EVAL "${a2}" "${env}"
              [[ "${__ERROR}" ]] && return 1
              ENV_SET "${env}" "${a1}" "${r}"
              return ;;
        let__STAR__) ENV "${env}"; local let_env="${r}"
              local let_pairs=(${ANON["${a1}"]})
              local idx=0
              #echo "let: [${let_pairs[*]}] for ${a2}"
              while [[ "${let_pairs["${idx}"]}" ]]; do
                  EVAL "${let_pairs[$(( idx + 1))]}" "${let_env}"
                  ENV_SET "${let_env}" "${let_pairs[${idx}]}" "${r}"
                  idx=$(( idx + 2))
              done
              ast="${a2}"
              env="${let_env}"
              # Continue loop
              ;;
        quote)
              r="${a1}"
              return ;;
        quasiquoteexpand)
              QUASIQUOTE "${a1}"
              return ;;
        quasiquote)
              QUASIQUOTE "${a1}"
              ast="${r}"
              # Continue loop
              ;;
        defmacro!)
              EVAL "${a2}" "${env}"
              [[ "${__ERROR}" ]] && return 1
              ANON["${r}_ismacro_"]="yes"
              ENV_SET "${env}" "${a1}" "${r}"
              return ;;
        macroexpand)
              MACROEXPAND "${a1}" "${env}"
              return ;;
        do)   _count "${ast}"
              _slice "${ast}" 1 $(( ${r} - 2 ))
              EVAL_AST "${r}" "${env}"
              [[ "${__ERROR}" ]] && r= && return 1
              _last "${ast}"
              ast="${r}"
              # Continue loop
              ;;
        if)   EVAL "${a1}" "${env}"
              [[ "${__ERROR}" ]] && return 1
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
        fn__STAR__)  _function "ENV \"${env}\" \"${a1}\" \"\${@}\"; \
                         EVAL \"${a2}\" \"\${r}\"" \
                        "${a2}" "${env}" "${a1}"
              return ;;
        *)    EVAL_AST "${ast}" "${env}"
              [[ "${__ERROR}" ]] && r= && return 1
              local el="${r}"
              _first "${el}"; local f="${ANON["${r}"]}"
              _rest "${el}"; local args="${ANON["${r}"]}"
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
ENV; REPL_ENV="${r}"
REP () {
    r=
    READ "${1}"
    EVAL "${r}" "${REPL_ENV}"
    PRINT "${r}"
}

# core.sh: defined using bash
_fref () {
    _symbol "${1}"; local sym="${r}"
    _function "${2} \"\${@}\""
    ENV_SET "${REPL_ENV}" "${sym}" "${r}"
}
for n in "${!core_ns[@]}"; do _fref "${n}" "${core_ns["${n}"]}"; done
_eval () { EVAL "${1}" "${REPL_ENV}"; }
_fref "eval" _eval
_list; argv="${r}"
for _arg in "${@:2}"; do _string "${_arg}"; _conj! "${argv}" "${r}"; done
_symbol "__STAR__ARGV__STAR__"
ENV_SET "${REPL_ENV}" "${r}" "${argv}";

# core.mal: defined using the language itself
REP "(def! not (fn* (a) (if a false true)))"
REP "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
REP "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"

# load/run file from command line (then exit)
if [[ "${1}" ]]; then
    REP "(load-file \"${1}\")"
    exit 0
fi 

# repl loop
while true; do
    READLINE "user> " || exit "$?"
    [[ "${r}" ]] && REP "${r}" && echo "${r}"
done

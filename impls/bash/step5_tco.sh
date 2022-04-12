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
_symbol DEBUG-EVAL; debug_eval="$r"

EVAL () {
    local ast="${1}" env="${2}"
    while true; do
    r=

    ENV_GET "$env" "$debug_eval"
    if [ -n "$__ERROR" ]; then
        __ERROR=
    elif [ "$r" != "$__false" -a "$r" != "$__nil" ]; then
        _pr_str "$ast" yes; echo "EVAL: $r / $env"
    fi

    _obj_type "${ast}"; local ot="${r}"
    case "${ot}" in
    symbol)
        ENV_GET "${env}" "${ast}"
        return ;;
    list)
        ;;
    vector)
        _map_with_type _vector EVAL "${ast}" "${env}"
        return ;;
    hash_map)
        local res="" key= val="" hm="${ANON["${ast}"]}"
        _hash_map; local new_hm="${r}"
        eval local keys="\${!${hm}[@]}"
        for key in ${keys}; do
            eval val="\${${hm}[\"${key}\"]}"
            EVAL "${val}" "${env}"
            _assoc! "${new_hm}" "${key}" "${r}"
        done
        r="${new_hm}"
        return ;;
    *)
        r="${ast}"
        return ;;
    esac

    # apply list
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
        do)   _count "${ast}"
              _slice "${ast}" 1 $(( ${r} - 2 ))
              _map_with_type _list EVAL "${r}" "${env}"
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
        *)    _map_with_type _list EVAL "${ast}" "${env}"
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

# core.mal: defined using the language itself
REP "(def! not (fn* (a) (if a false true)))"

# repl loop
while true; do
    READLINE "user> " || exit "$?"
    [[ "${r}" ]] && REP "${r}" && echo "${r}"
done

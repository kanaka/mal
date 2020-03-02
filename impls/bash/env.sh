#
# mal (Make a Lisp) environment definition
#

if [ -z "${__mal_env_included__}" ]; then
__mal_env_included=true

source $(dirname $0)/types.sh

# Any environment is a hash_map with an __outer__ key that refers to
# a parent environment (or nil)
ENV () {
    r=
    _hash_map
    local env="${r}"
    if [[ "${1}" ]]; then
        outer="${1}"; shift
        _assoc! "${env}" "__outer__" "${outer}"
    else
        _assoc! "${env}" "__outer__" "${__nil}"
    fi
    r="${env}"

    if [[ "${1}" && "${@}" ]]; then
        local binds=(${ANON["${1}"]}); shift
        local idx=0
        while [[ "${binds["${idx}"]}" ]]; do
            local fp="${ANON["${binds["${idx}"]}"]}"
            if [[ "${fp}" == "&" ]]; then
                idx=$(( idx + 1 ))
                fp="${ANON["${binds["${idx}"]}"]}"
                _list "${@}"
                _assoc! "${env}" "${fp}" "${r}"
                break
            else
                _assoc! "${env}" "${fp}" "${1}"
                shift
                idx=$(( idx + 1 ))
            fi
        done
    fi
    r="${env}"
}

# Find the environment with the key set and return the environment
ENV_FIND () {
    if _contains? "${1}" "${ANON["${2}"]}"; then
        r="${1}"
    else
        local obj="${ANON["${1}"]}"
        eval local outer="\${${obj}["__outer__"]}"
        if [[ "${outer}" && "${outer}" != "${__nil}" ]]; then
            ENV_FIND "${outer}" "${2}"
        else
            r=
        fi
    fi
}

# Find the environment with the key set and return the value of the
# key in that environment. If no environment contains the key then
# return an error
ENV_GET () {
    ENV_FIND "${1}" "${2}"
    local env="${r}"
    local key="${ANON["${2}"]}"
    if [[ "${r}" ]]; then
        local obj="${ANON["${env}"]}"
        eval r="\${${obj}["${key}"]}"
    else
        _error "'${key}' not found"
    fi
}

ENV_SET () {
    local key="${ANON["${2}"]}"
    _assoc! "${1}" "${key}" "${3}"
}

fi

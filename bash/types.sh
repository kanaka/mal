#
# mal (Make a Lisp) object types
#

if [ -z "${__mal_types_included__}" ]; then
__mal_types_included=true

declare -A ANON

__obj_magic=__5bal7
__keyw=$(echo -en "\xCA\x9E") # \u029E
__obj_hash_code=${__obj_hash_code:-0}

__new_obj_hash_code () {
    __obj_hash_code=$(( __obj_hash_code + 1))
    r="${__obj_hash_code}"
}

__new_obj () {
    __new_obj_hash_code
    r="${1}_${r}"
}

__new_obj_like () {
    __new_obj_hash_code
    r="${1%_*}_${r}"
}


# Errors/Exceptions

__ERROR=
_error() {
    _string "${1}"
    __ERROR="${r}"
    r=
}



#
# General functions
#

# Return the type of the object (or "make" if it's not a object
_obj_type () {
    local type="${1:0:4}"
    r=
    case "${type}" in
        symb) r="symbol" ;;
        list) r="list" ;;
        numb) r="number" ;;
        func) r="function" ;;
        strn)
            local s="${ANON["${1}"]}"
            if [[ "${1:0:1}" = "${__keyw}" ]] \
                || [[ "${1:0:2}" = "${__keyw}" ]]; then
                r="keyword"
            else
                r="string"
            fi ;;
        _nil) r="nil" ;;
        true) r="true" ;;
        fals) r="false" ;;
        vect) r="vector" ;;
        hmap) r="hash_map" ;;
        atom) r="atom" ;;
        undf) r="undefined" ;;
        *)    r="bash" ;;
    esac
}

_equal? () {
    _obj_type "${1}"; local ot1="${r}"
    _obj_type "${2}"; local ot2="${r}"
    if [[ "${ot1}" != "${ot2}" ]]; then
        if ! _sequential? "${1}" || ! _sequential? "${2}"; then
            return 1
        fi
    fi
    case "${ot1}" in
    string|symbol|keyword|number)
        [[ "${ANON["${1}"]}" == "${ANON["${2}"]}" ]] ;;
    list|vector|hash_map)
        _count "${1}"; local sz1="${r}"
        _count "${2}"; local sz2="${r}"
        [[ "${sz1}" == "${sz2}" ]] || return 1
        local a1=(${ANON["${1}"]})
        local a2=(${ANON["${2}"]})
        for ((i=0;i<${#a1[*]};i++)); do
            _equal? "${a1[${i}]}" "${a2[${i}]}" || return 1
        done
        ;;
    *)
        [[ "${1}" == "${2}" ]] ;;
    esac
}

# Constant atomic values

__nil=_nil_0
__true=true_0
__false=fals_0

_nil? () { [[ ${1} =~ ^_nil_ ]]; }
_true? () { [[ ${1} =~ ^true_ ]]; }
_false? () { [[ ${1} =~ ^fals_ ]]; }


# Symbols

_symbol () {
    __new_obj_hash_code
    r="symb_${r}"
    ANON["${r}"]="${1//\*/__STAR__}"
}
_symbol? () { [[ ${1} =~ ^symb_ ]]; }


# Keywords

_keyword () {
    local k="${1}"
    __new_obj_hash_code
    r="strn_${r}"
    if [[ "${1:0:1}" = "${__keyw}" ]] \
        || [[ "${1:0:2}" = "${__keyw}" ]]; then
        true
    else
        k="${__keyw}${1}"
    fi
    ANON["${r}"]="${k//\*/__STAR__}"
}
_keyword? () {
    [[ ${1} =~ ^strn_ ]] || return 1
    local s="${ANON["${1}"]}"
    [[ "${s:0:1}" = "${__keyw}" ]] || [[ "${s:0:2}" = "${__keyw}" ]]
}


# Numbers

_number () {
    __new_obj_hash_code
    r="numb_${r}"
    ANON["${r}"]="${1}"
}
_number? () { [[ ${1} =~ ^numb_ ]]; }


# Strings

_string () {
    __new_obj_hash_code
    r="strn_${r}"
    ANON["${r}"]="${1//\*/__STAR__}"
}
_string? () { [[ ${1} =~ ^strn_ ]]; }


# Functions
# Return a function object. The first parameter is the
# function 'source'.
_function () {
    __new_obj_hash_code
    eval "function ${__obj_magic}_func_${r} () { ${1%;} ; }"
    r="func_${r}"
    if [[ "${2}" ]]; then
        # Native function
        ANON["${r}"]="${__obj_magic}_${r}@${2}@${3}@${4}"
    else
        # Bash function
        ANON["${r}"]="${__obj_magic}_${r}"
    fi
}
_function? () { [[ ${1} =~ ^func_ ]]; }


# Lists

_list () {
    __new_obj_hash_code
    r="list_${r}"
    ANON["${r}"]="${*}"
}
_list? () { [[ ${1} =~ ^list_ ]]; }


# Vectors

_vector () {
    __new_obj_hash_code
    r="vector_${r}"
    ANON["${r}"]="${*}"
}
_vector? () { [[ ${1} =~ ^vector_ ]]; }


# hash maps (associative arrays)

_hash_map () {
    __new_obj_hash_code
    local name="hmap_${r}"
    local obj="${__obj_magic}_${name}"
    declare -A -g ${obj}; eval "${obj}=()"
    ANON["${name}"]="${obj}"

    while [[ "${1}" ]]; do
        eval ${obj}[\"${ANON["${1}"]}\"]=\"${2}\"
        shift; shift
    done

    r="${name}"
}
_hash_map? () { [[ ${1} =~ ^hmap_ ]]; }

_contains? () {
    local obj="${ANON["${1}"]}"
    eval [[ "\${${obj}[\"${2}\"]+isset}" ]]
}

_copy_hash_map () {
    local orig_obj="${ANON["${1}"]}"
    _hash_map
    local name="${r}"
    local obj="${ANON["${name}"]}"

    # Copy the existing key/values to the new object
    local temp=$(typeset -p ${orig_obj})
    eval ${temp/#declare -A ${orig_obj}=/declare -A -g ${obj}=}
    r="${name}"
}

# Return same hash map with keys/values added/mutated in place
_assoc! () {
    local obj=${ANON["${1}"]}; shift
    declare -A -g ${obj}

    # Set the key/values specified
    while [[ "${1}" ]]; do
        eval ${obj}[\"${1}\"]=\"${2}\"
        shift; shift
    done
}

# Return same hash map with keys/values deleted/mutated in place
_dissoc! () {
    local obj=${ANON["${1}"]}; shift
    declare -A -g ${obj}

    # Delete the key/values specified
    while [[ "${1}" ]]; do
        eval unset ${obj}[\"${1}\"]
        shift
    done
}


# Atoms

_atom() {
    __new_obj_hash_code
    r="atom_${r}"
    ANON["${r}"]="${*}"
}
_atom? () { [[ ${1} =~ ^atom_ ]]; }


# sequence operations

_sequential? () {
    _list? "${1}" || _vector? "${1}"
}

_nth () {
    local temp=(${ANON["${1}"]})
    r="${temp[${2}]}"
}

_first () {
    local temp="${ANON["${1}"]}"
    r="${temp%% *}"
    [ "${r}" ] || r="${__nil}"
}

_last () {
    local temp="${ANON["${1}"]}"
    r="${temp##* }"
}

# Creates a new vector/list of the everything after but the first
# element
_rest () {
    local temp="${ANON["${1}"]}"
    _list
    if [[ "${temp#* }" == "${temp}" ]]; then
        ANON["${r}"]=
    else
        ANON["${r}"]="${temp#* }"
    fi
}


_empty? () { [[ -z "${ANON["${1}"]}" ]]; }

# conj that mutates in place (and always appends)
_conj! () {
    local obj="${1}"; shift
    local obj_data="${ANON["${obj}"]}"
    ANON["${obj}"]="${obj_data:+${obj_data} }${*}"
    r="${1}"
}



_count () {
    if _nil? "${1}"; then
        r="0"
    else
        local temp=(${ANON["${1}"]})
        r=${#temp[*]}
    fi
}

# Slice a sequence object $1 starting at $2 of length $3
_slice () {
    local temp=(${ANON["${1}"]})
    __new_obj_like "${1}"
    ANON["${r}"]="${temp[@]:${2}:${3}}"
}

# Takes a bash function and an list object and invokes the function on
# each element of the list, returning a new list (or vector) of the results.
_map_with_type () {
    local constructor="${1}"; shift
    local f="${1}"; shift
    local items="${ANON["${1}"]}"; shift
    eval "${constructor}"; local new_seq="${r}"
    for v in ${items}; do
        #echo eval ${f%%@*} "${v}" "${@}"
        eval ${f%%@*} "${v}" "${@}"
        [[ "${__ERROR}" ]] && r= && return 1
        _conj! "${new_seq}" "${r}"
    done
    r="${new_seq}"
}

_map () {
    _map_with_type _list "${@}"
}

fi

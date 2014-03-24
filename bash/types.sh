#
# mal: Object Types and Functions
#

declare -A ANON

__obj_magic=__5bal7
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

__ERROR=


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
        strn) r="string" ;;
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

obj_type () {
    _obj_type "${1}"
    string "${r}"
}

_pr_str () {
    local print_readably="${2}"
    _obj_type "${1}"; local ot="${r}"
    if [[ -z "${ot}" ]]; then
        _error "_pr_str failed on '${1}'"
        r="<${1}>"
    else
        eval ${ot}_pr_str "${1}" "${print_readably}"
    fi
}

pr_str () {
    local res=""
    for x in "${@}"; do _pr_str "${x}" yes; res="${res} ${r}"; done
    string "${res:1}"
}

str () {
    local res=""
    for x in "${@}"; do _pr_str "${x}"; res="${res}${r}"; done
    string "${res}"
}

prn () {
    local res=""
    for x in "${@}"; do _pr_str "${x}" yes; res="${res} ${r}"; done
    echo "${res:1}"
    r="${__nil}"; 
}

println () {
    local res=""
    for x in "${@}"; do _pr_str "${x}"; res="${res} ${r}"; done
    res="${res//\\n/$'\n'}"
    echo -e "${res:1}"
    r="${__nil}"; 
}

with_meta () {
    local obj="${1}"; shift
    local meta_data="${1}"; shift
    __new_obj_like "${obj}"
    ANON["${r}"]="${ANON["${obj}"]}"
    local meta_obj="meta_${r#*_}"
    ANON["${meta_obj}"]="${meta_data}"
}

meta () {
    r="${ANON["meta_${1#*_}"]}"
    [[ "${r}" ]] || r="${__nil}"
}

#
# Constant atomic values
#

__undefined=undf_0
__nil=_nil_0
__true=true_0
__false=fals_0

_undefined? () { [[ ${1} =~ ^undf_ ]]; }
undefined? () { _undefined? "${1}" && r="${__true}" || r="${__false}"; }

_nil? () { [[ ${1} =~ ^_nil_ ]]; }
nil? () { _nil? "${1}" && r="${__true}" || r="${__false}"; }
nil_pr_str () { r="nil"; }

_true? () { [[ ${1} =~ ^true_ ]]; }
true? () { _true? "${1}" && r="${__true}" || r="${__false}"; }
true_pr_str () { r="true"; }

_false? () { [[ ${1} =~ ^fals_ ]]; }
false? () { _false? "${1}" && r="${__false}" || r="${__false}"; }
false_pr_str () { r="false"; }


#
# Numbers
#

number () {
    __new_obj_hash_code
    r="numb_${r}"
    ANON["${r}"]="${1}"
}
_number? () { [[ ${1} =~ ^numb_ ]]; }
number? () { _number? "${1}" && r="${__true}" || r="${__false}"; }
number_pr_str () { r="${ANON["${1}"]}"; }

num_plus     () { r=$(( ${ANON["${1}"]} + ${ANON["${2}"]} )); number "${r}"; }
num_minus    () { r=$(( ${ANON["${1}"]} - ${ANON["${2}"]} )); number "${r}"; }
num_multiply () { r=$(( ${ANON["${1}"]} * ${ANON["${2}"]} )); number "${r}"; }
num_divide   () { r=$(( ${ANON["${1}"]} / ${ANON["${2}"]} )); number "${r}"; }

_num_bool     () { [[ "${1}" = "1" ]] && r="${__true}" || r="${__false}"; }
num_gt       () { r=$(( ${ANON["${1}"]} >  ${ANON["${2}"]} )); _num_bool "${r}"; }
num_gte      () { r=$(( ${ANON["${1}"]} >= ${ANON["${2}"]} )); _num_bool "${r}"; }
num_lt       () { r=$(( ${ANON["${1}"]} <  ${ANON["${2}"]} )); _num_bool "${r}"; }
num_lte      () { r=$(( ${ANON["${1}"]} <= ${ANON["${2}"]} )); _num_bool "${r}"; }

#
# Symbols
#

symbol () {
    __new_obj_hash_code
    r="symb_${r}"
    ANON["${r}"]="${1//$'\*'/__STAR__}"
}
_symbol? () { [[ ${1} =~ ^symb_ ]]; }
symbol? () { _symbol? "${1}" && r="${__true}" || r="${__false}"; }
symbol_pr_str () {
    r="${ANON["${1}"]}"
    r="${r//__STAR__/*}"
}


#
# Strings
#

string () {
    __new_obj_hash_code
    r="strn_${r}"
    ANON["${r}"]="${1//$'\*'/__STAR__}"
}
_string? () { [[ ${1} =~ ^strn_ ]]; }
string? () { _string? "${1}" && r="${__true}" || r="${__false}"; }
string_pr_str () {
    local print_readably="${2}"
    if [ "${print_readably}" == "yes" ]; then
        local s="${ANON["${1}"]}"
        s="${s//\\/\\\\}"
        r="\"${s//\"/\\\"}\""
    else
        r="${ANON["${1}"]}"
    fi
    r="${r//__STAR__/$'*'}"
}

# TODO: subs


#
# Function objects
#

# Return a function object. The first parameter is the
# function 'source'.
new_function () {
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
function? () { _function? "${1}" && r="${__true}" || r="${__false}"; }
function_pr_str () { r="${ANON["${1}"]}"; }


#
# hash maps (associative arrays)
#

hash_map () {
    __new_obj_hash_code
    local name="hmap_${r}"
    local obj="${__obj_magic}_${name}"
    declare -A -g ${obj}
    ANON["${name}"]="${obj}"

    while [[ "${1}" ]]; do
        eval ${obj}[\"${ANON["${1}"]}\"]=\"${2}\"
        shift; shift
    done

    r="${name}"
}
_hash_map? () { [[ ${1} =~ ^hmap_ ]]; }
hash_map? () { _hash_map? "${1}" && r="${__true}" || r="${__false}"; }

hash_map_pr_str () {
    local print_readably="${2}"
    local res=""; local val=""
    local hm="${ANON["${1}"]}"
    eval local keys="\${!${hm}[@]}"
    for key in ${keys}; do
        #res="${res} \"${ANON["${key}"]}\""
        res="${res} \"${key//__STAR__/$'*'}\""
        eval val="\${${hm}[\"${key}\"]}"
        _pr_str "${val}" "${print_readably}"
        res="${res} ${r}"
    done
    r="{${res:1}}"
}

_copy_hash_map () {
    local orig_obj="${ANON["${1}"]}"
    hash_map
    local name="${r}"
    local obj="${ANON["${name}"]}"

    # Copy the existing key/values to the new object
    local temp=$(typeset -p ${orig_obj})
    eval ${temp/#declare -A ${orig_obj}=/declare -A -g ${obj}=}
    r="${name}"
}

# Return same hash map with keys/values added/mutated in place
assoc! () {
    local obj=${ANON["${1}"]}; shift
    declare -A -g ${obj}

    # Set the key/values specified
    while [[ "${1}" ]]; do
        eval ${obj}[\"${1}\"]=\"${2}\"
        shift; shift
    done
}

# Return same hash map with keys/values deleted/mutated in place
dissoc! () {
    local obj=${ANON["${1}"]}; shift
    declare -A -g ${obj}

    # Delete the key/values specified
    while [[ "${1}" ]]; do
        eval unset ${obj}[\"${1}\"]
        shift
    done
}

# Return new hash map with keys/values updated
assoc () {
    if ! _hash_map? "${1}"; then
        _error "assoc onto non-hash-map"
        return
    fi
    _copy_hash_map "${1}"; shift
    local name="${r}"
    local obj=${ANON["${name}"]}
    declare -A -g ${obj}

    while [[ "${1}" ]]; do
        eval ${obj}[\"${ANON["${1}"]}\"]=\"${2}\"
        shift; shift
    done
    r="${name}"
}

dissoc () {
    if ! _hash_map? "${1}"; then
        _error "dissoc from non-hash-map"
        return
    fi
    _copy_hash_map "${1}"; shift
    local name="${r}"
    local obj=${ANON["${name}"]}
    declare -A -g ${obj}

    while [[ "${1}" ]]; do
        eval unset ${obj}[\"${ANON["${1}"]}\"]
        shift
    done
    r="${name}"
}

_get () {
    _obj_type "${1}"; local ot="${r}"
    case "${ot}" in
    hash_map)
        local obj="${ANON["${1}"]}"
        eval r="\${${obj}[\"${2}\"]}" ;;
    list|vector)
        _nth "${1}" "${2}"
    esac
}
get () {
    _get "${1}" "${ANON["${2}"]}"
    [[ "${r}" ]] || r="${__nil}"
}

_contains? () {
    local obj="${ANON["${1}"]}"
    #echo "_contains? ${1} ${2} -> \${${obj}[\"${2}\"]+isset}"
    eval [[ "\${${obj}[\"${2}\"]+isset}" ]]
}
contains? () { _contains? "${1}" "${ANON["${2}"]}" && r="${__true}" || r="${__false}"; }

keys () {
    local obj="${ANON["${1}"]}"
    local kstrs=
    eval local keys="\${!${obj}[@]}"
    for k in ${keys}; do
        string "${k}"
        kstrs="${kstrs} ${r}"
    done

    __new_obj_hash_code
    r="list_${r}"
    ANON["${r}"]="${kstrs:1}"
}

vals () {
    local obj="${ANON["${1}"]}"
    local kvals=
    local val=
    eval local keys="\${!${obj}[@]}"
    for k in ${keys}; do
        eval val="\${${obj}["\${k}"]}"
        kvals="${kvals} ${val}"
    done

    __new_obj_hash_code
    r="list_${r}"
    ANON["${r}"]="${kvals:1}"
}

#
# Exceptions/Errors
#

_error() {
    string "${1}"
    __ERROR="${r}"
    r=
}
throw() {
    __ERROR="${1}"
    r=
}

#
# vectors
#

#
# vector (same as lists for now)
#

vector () {
    __new_obj_hash_code
    r="vector_${r}"
    ANON["${r}"]="${*}"
}
_vector? () { [[ ${1} =~ ^vector_ ]]; }
vector? () { _vector? "${1}" && r="${__true}" || r="${__false}"; }

vector_pr_str () {
    local print_readably="${2}"
    local res=""
    for elem in ${ANON["${1}"]}; do
        _pr_str "${elem}" "${print_readably}"
        res="${res} ${r}"
    done
    r="[${res:1}]"
}


#
# list (same as vectors for now)
#

list () {
    __new_obj_hash_code
    r="list_${r}"
    ANON["${r}"]="${*}"
}
_list? () { [[ ${1} =~ ^list_ ]]; }
list? () { _list? "${1}" && r="${__true}" || r="${__false}"; }

list_pr_str () {
    local print_readably="${2}"
    local res=""
    for elem in ${ANON["${1}"]}; do
        _pr_str "${elem}" "${print_readably}"
        res="${res} ${r}"
    done
    r="(${res:1})"
}

cons () {
    list ${1} ${ANON["${2}"]}
}


#
# atoms
#
atom() {
    __new_obj_hash_code
    r="atom_${r}"
    ANON["${r}"]="${*}"
}
_atom? () { [[ ${1} =~ ^atom_ ]]; }
atom? () { _atom? "${1}" && r="${__true}" || r="${__false}"; }
atom_pr_str () {
    local print_readably="${2}"
    _pr_str "${ANON["${1}"]}" "${print_readably}"
    r="(atom ${r})";
}
deref () {
    # TODO: double-check atom type
    r=${ANON["${1}"]}
}
reset_BANG () {
    local atm="${1}"; shift
    ANON["${atm}"]="${*}"
    r="${*}"
}
swap_BANG () {
    local atm="${1}"; shift
    local f="${ANON["${1}"]}"; shift
    ${f%%@*} "${ANON["${atm}"]}" "${@}"
    ANON["${atm}"]="${r}"
}


#
# sequence operations
#

_sequential? () {
    _list? "${1}" || _vector? "${1}"
}
sequential? () {
    _sequential? "${1}" && r="${__true}" || r="${__false}"
}

_nth () {
    local temp=(${ANON["${1}"]})
    r=${temp[${2}]}
}
nth () {
    _nth "${1}" "${ANON["${2}"]}"
}


_empty? () { [[ -z "${ANON["${1}"]}" ]]; }
empty? () { _empty? "${1}" && r="${__true}" || r="${__false}"; }

concat () {
    list
    local acc=""
    for item in "${@}"; do
        acc="${acc} ${ANON["${item}"]}"
    done
    ANON["${r}"]="${acc:1}"
}

conj () {
    local obj="${1}"; shift
    local obj_data="${ANON["${obj}"]}"
    __new_obj_like "${obj}"
    ANON["${r}"]="${obj_data:+${obj_data} }${*}"
}

# conj that mutates in place
conj! () {
    local obj="${1}"; shift
    local obj_data="${ANON["${obj}"]}"
    ANON["${obj}"]="${obj_data:+${obj_data} }${*}"
    r="${1}"
}



_count () {
    local temp=(${ANON["${1}"]})
    r=${#temp[*]}
}
count () {
    _count "${1}"
    number "${r}"
}

first () {
    local temp="${ANON["${1}"]}"
    r="${temp%% *}"
}

last () {
    local temp="${ANON["${1}"]}"
    r="${temp##* }"
}

# Slice a sequence object $1 starting at $2 of length $3
_slice () {
    local temp=(${ANON["${1}"]})
    __new_obj_like "${1}"
    ANON["${r}"]="${temp[@]:${2}:${3}}"
}

# Creates a new vector/list of the everything after but the first
# element
rest () {
    local temp="${ANON["${1}"]}"
    __new_obj_like "${1}"
    if [[ "${temp#* }" == "${temp}" ]]; then
        ANON["${r}"]=
    else
        ANON["${r}"]="${temp#* }"
    fi
}

apply () {
    local f="${ANON["${1}"]}"
    local args="${2}"
    local items="${ANON["${2}"]}"
    eval ${f%%@*} ${items}
}

# Takes a bash function and an list object and invokes the function on
# each element of the list, returning a new list (or vector) of the results.
_map_with_type () {
    local ot="${1}"; shift
    local f="${1}"; shift
    local items="${ANON["${1}"]}"; shift
    eval "${ot}"; local new_seq="${r}"
    for v in ${items}; do
        #echo eval ${f%%@*} "${v}" "${@}"
        eval ${f%%@*} "${v}" "${@}"
        [[ "${__ERROR}" ]] && r= && return 1
        conj! "${new_seq}" "${r}"
    done
    r="${new_seq}"
}

_map () {
    _map_with_type list "${@}"
}

# Takes a function object and an list object and invokes the function
# on each element of the list, returning a new list of the results.
map () {
    local f="${ANON["${1}"]}"; shift
    #echo _map "${f}" "${@}"
    _map "${f}" "${@}"
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
    string|symbol|number)
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
equal? () {
    _equal? "${1}" "${2}" && r="${__true}" || r="${__false}"
}
 
#
# ENV
#

# Any environment is a hash_map with an __outer__ key that refers to
# a parent environment (or nil)
ENV () {
    r=
    hash_map
    local env="${r}"
    if [[ "${1}" ]]; then
        outer="${1}"; shift
        assoc! "${env}" "__outer__" "${outer}"
    else
        assoc! "${env}" "__outer__" "${__nil}"
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
                list "${@}"
                assoc! "${env}" "${fp}" "${r}"
                break
            else
                assoc! "${env}" "${fp}" "${1}"
                shift
                idx=$(( idx + 1 ))
            fi
        done
    fi
    r="${env}"
}

# Find the environment with the key set and return the environment
ENV_FIND () {
    if _contains? "${1}" "${2}"; then
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
    if [[ "${r}" ]]; then
        local obj="${ANON["${env}"]}"
        eval r="\${${obj}["${2}"]}"
    else
        _error "'${2}' not found"
    fi
}

ENV_SET () {
    assoc! "${1}" "${2}" "${3}"
}

# TODO: memory visualizer (like Make implementation)

# Namespace of type functions

declare -A types_ns=(
    [type]=obj_type
    [pr-str]=pr_str [str]=str [prn]=prn [println]=println
    [with-meta]=with_meta [meta]=meta
    [=]=equal?
    [nil?]=nil? [true?]=true? [false?]=false?
    [symbol?]=symbol?
    [>]=num_gt [>=]=num_gte [<]=num_lt [<=]=num_lte
    [+]=num_plus [-]=num_minus [__STAR__]=num_multiply [/]=num_divide
    [hash-map]=hash_map [map?]=hash_map?
    [assoc]=assoc [dissoc]=dissoc [get]=get
    [contains?]=contains? [keys]=keys [vals]=vals
    [throw]=throw
    [list]=list [list?]=list?
    [vector]=vector [vector?]=vector?
    [atom]=atom [atom?]=atom? [deref]=deref
    [reset!]=reset_BANG [swap!]=swap_BANG
    [sequential?]=sequential?
    [cons]=cons [nth]=nth [count]=count [empty?]=empty?
    [concat]=concat [conj]=conj [first]=first [rest]=rest
    [apply]=apply [map]=map)

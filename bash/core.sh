#
# mal (Make a Lisp) object types
#

if [ -z "${__mal_core_included__}" ]; then
__mal_core_included=true

source $(dirname $0)/types.sh
source $(dirname $0)/reader.sh
source $(dirname $0)/printer.sh

# Exceptions/Errors

throw() {
    __ERROR="${1}"
    r=
}


# General functions

obj_type () {
    _obj_type "${1}"
    _string "${r}"
}

equal? () {
    _equal? "${1}" "${2}" && r="${__true}" || r="${__false}"
}


# Scalar functions

nil? () { _nil? "${1}" && r="${__true}" || r="${__false}"; }
true? () { _true? "${1}" && r="${__true}" || r="${__false}"; }
false? () { _false? "${1}" && r="${__true}" || r="${__false}"; }


# Symbol functions

symbol? () { _symbol? "${1}" && r="${__true}" || r="${__false}"; }


# Number functions

number? () { _number? "${1}" && r="${__true}" || r="${__false}"; }

num_plus     () { r=$(( ${ANON["${1}"]} + ${ANON["${2}"]} )); _number "${r}"; }
num_minus    () { r=$(( ${ANON["${1}"]} - ${ANON["${2}"]} )); _number "${r}"; }
num_multiply () { r=$(( ${ANON["${1}"]} * ${ANON["${2}"]} )); _number "${r}"; }
num_divide   () { r=$(( ${ANON["${1}"]} / ${ANON["${2}"]} )); _number "${r}"; }

_num_bool     () { [[ "${1}" = "1" ]] && r="${__true}" || r="${__false}"; }
num_gt       () { r=$(( ${ANON["${1}"]} >  ${ANON["${2}"]} )); _num_bool "${r}"; }
num_gte      () { r=$(( ${ANON["${1}"]} >= ${ANON["${2}"]} )); _num_bool "${r}"; }
num_lt       () { r=$(( ${ANON["${1}"]} <  ${ANON["${2}"]} )); _num_bool "${r}"; }
num_lte      () { r=$(( ${ANON["${1}"]} <= ${ANON["${2}"]} )); _num_bool "${r}"; }

# return number of milliseconds since epoch
time_ms () {
    local ms=$(date +%s%3N)
    _number "${ms}"
}


# String functions

string? () { _string? "${1}" && r="${__true}" || r="${__false}"; }

pr_str () {
    local res=""
    for x in "${@}"; do _pr_str "${x}" yes; res="${res} ${r}"; done
    _string "${res:1}"
}

str () {
    local res=""
    for x in "${@}"; do _pr_str "${x}"; res="${res}${r}"; done
    _string "${res}"
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

readline () {
    READLINE "${ANON["${1}"]}" && _string "${r}" || r="${__nil}"
}

read_string () {
    READ_STR "${ANON["${1}"]}"
}

slurp () {
    local lines
    mapfile lines < "${ANON["${1}"]}"
    local text="${lines[*]}"; text=${text//$'\n' /$'\n'}
    _string "${text}"
}


# Function functions
function? () { _function? "${1}" && r="${__true}" || r="${__false}"; }


# List functions
list? () { _list? "${1}" && r="${__true}" || r="${__false}"; }


# Vector functions (same as lists for now)
vector? () { _vector? "${1}" && r="${__true}" || r="${__false}"; }


# Hash map (associative array) functions
hash_map? () { _hash_map? "${1}" && r="${__true}" || r="${__false}"; }

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
        _nth "${1}" "${2}" ;;
    nil)
        r="${__nil}" ;;
    esac
}
get () {
    _get "${1}" "${ANON["${2}"]}"
    [[ "${r}" ]] || r="${__nil}"
}

contains? () { _contains? "${1}" "${ANON["${2}"]}" && r="${__true}" || r="${__false}"; }

keys () {
    local obj="${ANON["${1}"]}"
    local kstrs=
    eval local keys="\${!${obj}[@]}"
    for k in ${keys}; do
        _string "${k}"
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


# sequence operations

sequential? () {
    _sequential? "${1}" && r="${__true}" || r="${__false}"
}

cons () {
    _list ${1} ${ANON["${2}"]}
}

concat () {
    _list
    local acc=""
    for item in "${@}"; do
        acc="${acc} ${ANON["${item}"]}"
    done
    ANON["${r}"]="${acc:1}"
}

nth () {
    _nth "${1}" "${ANON["${2}"]}"
}

empty? () { _empty? "${1}" && r="${__true}" || r="${__false}"; }

count () {
    _count "${1}"
    _number "${r}"
}

conj () {
    local obj="${1}"; shift
    local obj_data="${ANON["${obj}"]}"
    __new_obj_like "${obj}"
    if _list? "${obj}"; then
        ANON["${r}"]="${obj_data:+${obj_data}}"
        for elem in ${@}; do
            ANON["${r}"]="${elem} ${ANON["${r}"]}"
        done

    else
        ANON["${r}"]="${obj_data:+${obj_data} }${*}"
    fi
}

apply () {
    local f="${ANON["${1}"]}"; shift
    local items="${@:1:$(( ${#@} -1 ))} ${ANON["${!#}"]}"
    eval ${f%%@*} ${items}
}

# Takes a function object and an list object and invokes the function
# on each element of the list, returning a new list of the results.
map () {
    local f="${ANON["${1}"]}"; shift
    #echo _map "${f}" "${@}"
    _map "${f}" "${@}"
}


# Metadata functions

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


# Atom functions

atom? () { _atom? "${1}" && r="${__true}" || r="${__false}"; }
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



# Namespace of core functions

declare -A core_ns=(
    [type]=obj_type
    [=]=equal?
    [throw]=throw
    [nil?]=nil?
    [true?]=true?
    [false?]=false?
    [symbol?]=symbol?

    [pr-str]=pr_str
    [str]=str
    [prn]=prn
    [println]=println
    [readline]=readline
    [read-string]=read_string
    [slurp]=slurp
    [<]=num_lt
    [<=]=num_lte
    [>]=num_gt
    [>=]=num_gte
    [+]=num_plus
    [-]=num_minus
    [__STAR__]=num_multiply
    [/]=num_divide
    [time-ms]=time_ms

    [list]=_list
    [list?]=list?
    [vector]=_vector
    [vector?]=vector?
    [hash-map]=_hash_map
    [map?]=hash_map?
    [assoc]=assoc
    [dissoc]=dissoc
    [get]=get
    [contains?]=contains?
    [keys]=keys
    [vals]=vals

    [sequential?]=sequential?
    [cons]=cons
    [concat]=concat
    [nth]=nth
    [first]=_first
    [rest]=_rest
    [empty?]=empty?
    [count]=count
    [conj]=conj
    [apply]=apply
    [map]=map

    [with-meta]=with_meta
    [meta]=meta
    [atom]=_atom
    [atom?]=atom?
    [deref]=deref
    [reset!]=reset_BANG
    [swap!]=swap_BANG)

fi

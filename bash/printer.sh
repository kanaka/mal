#
# mal (Make a Lisp) printer
#

if [ -z "${__mal_printer_included__}" ]; then
__mal_printer_included=true

source $(dirname $0)/types.sh

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

nil_pr_str () { r="nil"; }
true_pr_str () { r="true"; }
false_pr_str () { r="false"; }

number_pr_str () { r="${ANON["${1}"]}"; }

symbol_pr_str () {
    r="${ANON["${1}"]}"
    r="${r//__STAR__/*}"
}

keyword_pr_str () {
    string_pr_str "${1}"
}

_raw_string_pr_str () {
    local s="${1}"
    local print_readably="${2}"
    if [[ "${s:0:1}" = "${__keyw}" ]]; then
        r=":${s:1}"
    elif [ "${print_readably}" == "yes" ]; then
        s="${s//\\/\\\\}"
        r="\"${s//\"/\\\"}\""
    else
        r="${s}"
    fi
    r="${r//__STAR__/$'*'}"
}

string_pr_str () {
    _raw_string_pr_str "${ANON["${1}"]}" "${2}"
}

function_pr_str () { r="${ANON["${1}"]}"; }

bash_pr_str () {
    r="$(declare -f -p ${1})"
}

hash_map_pr_str () {
    local print_readably="${2}"
    local res=""; local val=""
    local hm="${ANON["${1}"]}"
    eval local keys="\${!${hm}[@]}"
    for key in ${keys}; do
        _raw_string_pr_str "${key}" "${print_readably}"
        res="${res} ${r}"
        eval val="\${${hm}[\"${key}\"]}"
        _pr_str "${val}" "${print_readably}"
        res="${res} ${r}"
    done
    r="{${res:1}}"
}

vector_pr_str () {
    local print_readably="${2}"
    local res=""
    for elem in ${ANON["${1}"]}; do
        _pr_str "${elem}" "${print_readably}"
        res="${res} ${r}"
    done
    r="[${res:1}]"
}

list_pr_str () {
    local print_readably="${2}"
    local res=""
    for elem in ${ANON["${1}"]}; do
        _pr_str "${elem}" "${print_readably}"
        res="${res} ${r}"
    done
    r="(${res:1})"
}

atom_pr_str () {
    local print_readably="${2}"
    _pr_str "${ANON["${1}"]}" "${print_readably}"
    r="(atom ${r})";
}

fi

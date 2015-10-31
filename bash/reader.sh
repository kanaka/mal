#
# mal (Make Lisp) Parser/Reader
#

if [ -z "${__mal_readerr_included__}" ]; then
__mal_readerr_included=true

source $(dirname $0)/types.sh

READ_ATOM () {
    local token=${__reader_tokens[${__reader_idx}]}
    __reader_idx=$(( __reader_idx + 1 ))
    case "${token}" in
        [0-9]*) _number "${token}" ;;
        \"*)    token="${token:1:-1}"
                token="${token//\\\\/\\}"
                token="${token//\\\"/\"}"
                token="${token//\\n/$'\n'}"
                _string "${token}" ;;
        :*)     _keyword "${token:1}" ;;
        nil)    r="${__nil}" ;;
        true)   r="${__true}" ;;
        false)  r="${__false}" ;;
        *)      _symbol "${token}" ;;
    esac
}

# Return seqence of tokens into r.
#   ${1}: Type of r (vector, list)
#   ${2}: starting symbol
#   ${3}: ending symbol
READ_SEQ () {
    local start="${1}"
    local end="${2}"
    local items=""
    local token=${__reader_tokens[${__reader_idx}]}
    __reader_idx=$(( __reader_idx + 1 ))
    if [[ "${token}" != "${start}" ]]; then
        r=
        _error "expected '${start}'"
        return
    fi
    token=${__reader_tokens[${__reader_idx}]}
    while [[ "${token}" != "${end}" ]]; do
        if [[ ! "${token}" ]]; then
            r=
            _error "exepected '${end}', got EOF"
            return
        fi
        READ_FORM
        items="${items} ${r}"
        token=${__reader_tokens[${__reader_idx}]}
    done
    __reader_idx=$(( __reader_idx + 1 ))
    r="${items:1}"
}

# Return form in r
READ_FORM () {
    local token=${__reader_tokens[${__reader_idx}]}
    case "${token}" in
        \')     __reader_idx=$(( __reader_idx + 1 ))
                _symbol quote; local q="${r}"
                READ_FORM; local f="${r}"
                _list "${q}" "${f}" ;;
        \`)     __reader_idx=$(( __reader_idx + 1 ))
                _symbol quasiquote; local q="${r}"
                READ_FORM; local f="${r}"
                _list "${q}" "${f}" ;;
        \~)     __reader_idx=$(( __reader_idx + 1 ))
                _symbol unquote; local q="${r}"
                READ_FORM; local f="${r}"
                _list "${q}" "${f}" ;;
        \~\@)   __reader_idx=$(( __reader_idx + 1 ))
                _symbol splice-unquote; local q="${r}"
                READ_FORM; local f="${r}"
                _list "${q}" "${f}" ;;
        ^)      __reader_idx=$(( __reader_idx + 1 ))
                _symbol with-meta; local wm="${r}"
                READ_FORM; local meta="${r}"
                READ_FORM; local obj="${r}"
                _list "${wm}" "${obj}" "${meta}" ;;
        @)     __reader_idx=$(( __reader_idx + 1 ))
                _symbol deref; local d="${r}"
                READ_FORM; local f="${r}"
                _list "${d}" "${f}" ;;
        \))     _error "unexpected ')'" ;; 
        \()     READ_SEQ "(" ")"
                _list ${r} ;;
        \])     _error "unexpected ']'" ;; 
        \[)     READ_SEQ "[" "]"
                _vector ${r} ;;
        \})     _error "unexpected '}'" ;; 
        \{)     READ_SEQ "{" "}"
                _hash_map ${r} ;;
        *)      READ_ATOM
    esac
}

# Returns __reader_tokens as an indexed array of tokens
TOKENIZE () {
    local data="${*}"
    local datalen=${#data}
    local idx=0
    local chunk=0
    local chunksz=500
    local match=
    local token=
    local str=

    __reader_idx=0
    __reader_tokens=
    while true; do
        if (( ${#str} < ( chunksz / 2) )) && (( chunk < datalen )); then
            str="${str}${data:${chunk}:${chunksz}}"
            chunk=$(( chunk + ${chunksz} ))
        fi
        (( ${#str} == 0 )) && break
        [[ "${str}" =~ ^^([][{}\(\)^@])|^(~@)|(\"(\\.|[^\\\"])*\")|^(;[^$'\n']*)|^([~\'\`])|^([^][ ~\`\'\";{}\(\)^@\,]+)|^[,]|^[[:space:]]+ ]]
        match=${BASH_REMATCH[0]}
        str="${str:${#match}}"
        token="${match//$'\n'/}"
        #echo "MATCH: '${token}' / [${str}]"
        if ! [[ "${token}" =~ (^[,]$|^[[:space:]]*;.*$|^[[:space:]]*$) ]]; then 
            __reader_tokens[${idx}]="${token}"
            idx=$(( idx + 1 ))
        fi
        if [ -z "${match}" ]; then
            _error "Tokenizing error at: ${str:0:50}"
            return 1
        fi
    done
}

# read-str from a raw "string" or from a string object. Retruns object
# read in r.
READ_STR () {
    declare -a __reader_tokens
    TOKENIZE "${*}" || return 1  # sets __reader_tokens
    #set | grep ^__reader_tokens
    if [ -z "${__reader_tokens[0]}" ]; then
        r=
        return 1  # No tokens
    fi
    READ_FORM
    #echo "Token: ${r}: <${ANON["${r}"]}>"
    return
}

# Call readline and save the history. Returns the string read in r.
READLINE_EOF=
READLINE_HISTORY_FILE=${HOME}/.mal-history
READLINE () {
    history -r "${READLINE_HISTORY_FILE}" 2>/dev/null || true
    read -r -e -p "${1}" r || return "$?"
    history -s -- "${r}"
    history -a "${READLINE_HISTORY_FILE}" 2>/dev/null || true
}

fi

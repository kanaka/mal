#!/bin/bash

set -e

DEBUG=${DEBUG:-}
KEEP_REM=${KEEP_REM:-}
KEEP_REM_LABELS=${KEEP_REM_LABELS:-}

infile=$1

die () {
    echo >&2 "$*"
    exit 1
}

[ "${infile}" ] || die "Usage: <infile>"

input=$(cat ${infile})

[ "${DEBUG}" ] && echo >&2 "Processing includes"

full="${input}"
declare -A included

while [[ ${input} =~ REM\ \$INCLUDE:\ \'.*\' ]]; do
    full=""
    while read -r line; do
        if [[ ${line} =~ REM\ \$INCLUDE:\ \'.*\' ]]; then
            include=${line#REM \$INCLUDE: \'}
            include=${include%\'}
            # Only include it once
            if [ "${included[${include}]}" ];then
                [ "${DEBUG}" ] && echo >&2 "already included: ${include}"
                continue
            fi
            [ "${DEBUG}" ] && echo >&2 "including: ${include}"
            included[${include}]="done"
            if [ "${KEEP_REM}" ]; then
                full="${full}\nREM vvv BEGIN '${include}' vvv\n$(cat ${include})\nREM vvv END '${include}' vvv\n"
            else
                full="${full}\n$(cat ${include})\n"
            fi
        else
            full="${full}${line}\n"
        fi
    done < <(echo -e "${input}")
    input="${full}"
done


[ "${DEBUG}" ] && echo >&2 "Renumbering"

data=""
declare -A labels

lnum=10
while read -r line; do
    if [[ ${line} =~ ^\ *# ]]; then
        [ "${DEBUG}" ] && echo >&2 "ignoring # style comment at $lnum"
        continue
    elif [[ -z "${KEEP_REM}" && ${line} =~ ^\ *REM ]]; then
        [ "${DEBUG}" ] && echo >&2 "dropping REM comment: ${line}"
        continue
    elif [[ ${line} =~ ^\ *$ ]]; then
            [ "${DEBUG}" ] && echo >&2 "found blank line at $lnum"
            data="${data}\n"
            continue
    elif [[ ${line} =~ ^[A-Za-z_][A-Za-z0-9_]*:$ ]]; then
        label=${line%:}
        [ "${DEBUG}" ] && echo >&2 "found label ${label} at $lnum"
        labels[${label}]=$lnum
        if [ -n "${KEEP_REM_LABELS}" ]; then
            data="${data}${lnum} REM ${label}:\n"
        else
            continue
        fi
    else
        data="${data}${lnum} ${line}\n"
    fi
    lnum=$(( lnum + 10 ))
done < <(echo -e "${input}")

if [[ -z "${KEEP_REM}" ]]; then
    [ "${DEBUG}" ] && echo >&2 "Dropping line ending REMs"
    data=$(echo -e "${data}" | sed "s/: REM [^\n]*$//")
fi

for label in "${!labels[@]}"; do
    [ "${DEBUG}" ] && echo >&2 "Updating label: ${label}"
    lnum=${labels[${label}]}
    if [ -n "${KEEP_REM_LABELS}" ]; then
        data=$(echo "${data}" | sed "s/\(THEN\|GOTO\|GOSUB\) ${label}\>/\1 ${lnum}: REM ${label}/g")
    else
        data=$(echo "${data}" | sed "s/\(THEN\|GOTO\|GOSUB\) ${label}\>/\1 ${lnum}/g")
    fi
done

echo -e "${data}"

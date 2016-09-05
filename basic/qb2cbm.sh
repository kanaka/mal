#!/bin/bash

set -e

DEBUG=${DEBUG:-}

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
            full="${full}\nREM vvv BEGIN '${include}' vvv\n$(cat ${include})\nREM vvv END '${include}' vvv\n"
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
    if [[ ${line} =~ ^\ *$ ]]; then
            [ "${DEBUG}" ] && echo >&2 "found blank line after $lnum"
            data="${data}\n"
            continue
    elif [[ ${line} =~ ^[A-Za-z_]*:$ ]]; then
        label=${line%:}
        [ "${DEBUG}" ] && echo >&2 "found label ${label} at $lnum"
        labels[${label}]=$lnum
        data="${data}${lnum} REM ${label}:\n"
    else
        data="${data}${lnum} ${line}\n"
    fi
    lnum=$(( lnum + 10 ))
done < <(echo -e "${input}")

for label in "${!labels[@]}"; do
    [ "${DEBUG}" ] && echo >&2 "Updating label: ${label}"
    lnum=${labels[${label}]}
    data=$(echo "${data}" | sed "s/\(THEN\|GOTO\|GOSUB\) ${label}\>/\1 ${lnum}: REM \1 ${label}/g")
done

echo -en "${data}"

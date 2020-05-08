#!/bin/bash

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"

ORACLE_LOGON=${ORACLE_LOGON:-system/oracle}
SQLPLUS="sqlplus -S ${ORACLE_LOGON}"

FILE_PID=
cleanup() {
    trap - TERM QUIT INT EXIT
    #echo cleanup: ${FILE_PID}
    [ "${FILE_PID}" ] && kill ${FILE_PID}
}
trap "cleanup" TERM QUIT INT EXIT


# Load the SQL code
if [ -z "${SKIP_INIT}" ]; then
    out=$(echo "" | ${SQLPLUS} @$1)
    if echo "${out}" | grep -vs "^No errors.$" \
        | grep -si error >/dev/null; then
    #if echo "${out}"  | grep -si error >/dev/null; then
        echo "${out}"
        exit 1
    fi
fi

# open I/O streams
echo -e "BEGIN io.open(0); io.open(1); END;\n/" \
    | ${SQLPLUS} >/dev/null

# Stream from table to stdout
(
while true; do
    out="$(echo "SELECT io.read(1) FROM dual;" \
        | ${SQLPLUS} 2>/dev/null)" || break
    #echo "out: [${out}] (${#out})"
    echo "${out}"
done
) &
STDOUT_PID=$!

# Perform readline input into stream table when requested
(
[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE}
while true; do
    prompt=$(echo "SELECT io.wait_rl_prompt(0) FROM dual;" \
        | ${SQLPLUS} 2>/dev/null) || break
    # Prompt is returned single-quoted because sqlplus trims trailing
    # whitespace. Remove the single quotes from the beginning and end:
    prompt=${prompt%\'}
    prompt=${prompt#\'}
    #echo "prompt: [${prompt}]"

    IFS= read -u 0 -r -e -p "${prompt}" line || break
    if [ "${line}" ]; then
        history -s -- "${line}"        # add to history
        history -a ${RL_HISTORY_FILE}  # save history to file
    fi

    # Escape (double) single quotes per SQL norm
    line=${line//\'/\'\'}
    #echo "line: [${line}]"
    ( echo -n "BEGIN io.writeline('${line}', 0); END;";
      echo -en "\n/" ) \
        | ${SQLPLUS} >/dev/null || break
done
echo -e "BEGIN io.close(0); END;\n/" \
    | ${SQLPLUS} > /dev/null
) <&0 >&1 &


# File read if requested
(
while true; do
    files="$(echo "SELECT path FROM file_io WHERE in_or_out = 'in';" \
        | ${SQLPLUS} 2>/dev/null \
        | grep -v "^no rows selected")" || break
    for f in ${files}; do
        if [ ! -r ${f} ]; then
            echo "UPDATE file_io SET error = 'Cannot read ''${f}''' WHERE path = '${f}' AND in_or_out = 'in';" \
                | ${SQLPLUS} >/dev/null
            continue;
        fi
        IFS= read -rd '' content < "${f}"
        # sqlplus limits lines to 2499 characters so split the update
        # into chunks of the file ORed together over multiple lines
        query="UPDATE file_io SET data = TO_CLOB('')"
        while [ -n "${content}" ]; do
            chunk="${content:0:2000}"
            content="${content:${#chunk}}"
            chunk="${chunk//\'/\'\'}"
            chunk="${chunk//$'\n'/\\n}"
            query="${query}"$'\n'"    || TO_CLOB('${chunk}')"
        done
        query="${query}"$'\n'" WHERE path = '${f}' AND in_or_out = 'in';"
        echo "${query}" | ${SQLPLUS} > /dev/null
        #echo "file read: ${f}: ${?}"
    done
    sleep 1
done
) &
FILE_PID=$!

res=0
shift
if [ $# -gt 0 ]; then
    # If there are command line arguments then run a command and exit
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    echo -e "SELECT mal.MAIN('(${args})') FROM dual;" \
        | ${SQLPLUS} > /dev/null
    res=$?
else
    # Start main loop in the background
    echo "SELECT mal.MAIN() FROM dual;" \
        | ${SQLPLUS} > /dev/null
    res=$?
fi
# Wait for output to flush
wait ${STDOUT_PID}
exit ${res}

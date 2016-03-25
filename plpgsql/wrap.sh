#!/bin/bash

set -e

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL="psql -q -t -A -v ON_ERROR_STOP=1"
[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

MAIN_PID=
STDOUT_PID=

cleanup() {
    echo "cleanup"
    trap - TERM QUIT INT EXIT
    [ "${MAIN_PID}" ] && kill ${MAIN_PID}
    [ "${STDOUT_PID}" ] && kill ${STDOUT_PID}
}
trap "cleanup" TERM QUIT INT EXIT

# Load the SQL code
[ "${SKIP_INIT}" ] || ${PSQL} -f $1 >/dev/null

# Set the present working directory (for slurp)
# TODO: not "multiprocess" safe.
${PSQL} -dmal -c "SELECT env_vset(0, '*PWD*', READ('$(pwd)'));" >/dev/null

shift
if [ $# -gt 0 ]; then
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    ${PSQL} -dmal -v arg1="(${args})" -f <(echo "SELECT RUN(:'arg1');")
    exit 0
fi

# Start main loop in the background
(
    ${PSQL} -dmal -f <(echo "SELECT MAIN_LOOP();")
) &
MAIN_PID=$!

# Stream from table to stdout
(
    while true; do
        out=$(${PSQL} -dmal -f <(echo "SELECT read(1);"))
        #echo "here stdout: [$out]" >> /tmp/debug.inout
        #echo -en "${out}"
        echo "${out}"
    done
) &
STDOUT_PID=$!

# Perform readline input into stream table when requested
[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE} 
while true; do
    prompt=$(${PSQL} -dmal -f <(echo "SELECT wait_rl_prompt(0);"))
    read -u 0 -r -e -p "${prompt}" line || break
    #echo "here stdin: [$line]" >> /tmp/debug.inout
    if [ "${line}" ]; then
        history -s -- "${line}"        # add to history
        history -a ${RL_HISTORY_FILE}  # save history to file
    fi

    ${PSQL} -dmal -v arg="${line}" \
        -f <(echo "SELECT writeline(:'arg', 0);") >/dev/null
done

cleanup

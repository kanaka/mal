#!/bin/bash

set -e

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL="psql -q -t -A -v ON_ERROR_STOP=1"
[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

MAIN_PID=
STDOUT_PID=

cleanup() {
    trap - TERM QUIT INT EXIT
    [ "${MAIN_PID}" ] && kill ${MAIN_PID}
    [ "${STDOUT_PID}" ] && kill ${STDOUT_PID}
}
trap "cleanup" TERM QUIT INT EXIT

# Load the SQL code
[ "${SKIP_INIT}" ] || ${PSQL} -f $1 >/dev/null

# Stream from table to stdout
( while true; do echo "$(${PSQL} -dmal -c "SELECT read(1)")"; done ) &
STDOUT_PID=$!

# If there are command line arguments then run a command and exit
shift
if [ $# -gt 0 ]; then
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    ${PSQL} -dmal -v args="(${args})" \
        -f <(echo "SELECT RUN('$(pwd)', :'args');") > /dev/null
    sleep 2  # TODO: fix this
    exit $?
fi

# Start main loop in the background
( ${PSQL} -dmal -c "SELECT MAIN_LOOP('$(pwd)');" ) &
MAIN_PID=$!

# Perform readline input into stream table when requested
[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE}
while true; do
    prompt=$(${PSQL} -dmal -c "SELECT wait_rl_prompt(0);")
    read -u 0 -r -e -p "${prompt}" line || break
    if [ "${line}" ]; then
        history -s -- "${line}"        # add to history
        history -a ${RL_HISTORY_FILE}  # save history to file
    fi

    ${PSQL} -dmal -v arg="${line}" \
        -f <(echo "SELECT writeline(:'arg', 0);") >/dev/null
done

cleanup

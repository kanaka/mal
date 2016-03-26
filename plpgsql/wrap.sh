#!/bin/bash

set -e

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL="psql -q -t -A -v ON_ERROR_STOP=1"
[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

STDIN_PID=
STDOUT_PID=

cleanup() {
    #echo "in cleanup"
    trap - TERM QUIT INT EXIT
    [ "${STDIN_PID}" ] && kill ${STDIN_PID} 2>/dev/null || true
    [ "${STDOUT_PID}" ] && kill ${STDOUT_PID} 2>/dev/null || true
}
trap "cleanup" TERM QUIT INT EXIT

# Load the SQL code
[ "${SKIP_INIT}" ] || ${PSQL} -f $1 >/dev/null

# Stream from table to stdout
(
while true; do
    echo "$(${PSQL} -dmal -c "SELECT read_or_error(1)")" || break
done
) &
STDOUT_PID=$!

# Perform readline input into stream table when requested
${PSQL} -dmal -c "SELECT stream_open(0);" > /dev/null
(
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
${PSQL} -dmal -c "SELECT stream_close(0);" > /dev/null
) <&0 >&1 &
STDIN_PID=$!

shift
if [ $# -gt 0 ]; then
    # If there are command line arguments then run a command and exit
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    ${PSQL} -dmal -v args="(${args})" \
        -f <(echo "SELECT RUN('$(pwd)', :'args');") > /dev/null
    exit $?
else
    # Start main loop in the background
    ${PSQL} -dmal -c "SELECT MAIN_LOOP('$(pwd)');"
fi

cleanup

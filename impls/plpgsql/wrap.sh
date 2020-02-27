#!/bin/bash

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL_USER="${PSQL_USER:-postgres}"

PSQL="psql -q -t -A -v ON_ERROR_STOP=1 ${PSQL_USER:+-U ${PSQL_USER}}"
[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

# If mal DB is not there, force create of it
dbcheck=$(${PSQL} -c "select 1 from pg_database where datname='mal'")
[ -z "${dbcheck}" ] && SKIP_INIT=

STDOUT_PID= STDIN_PID=
cleanup () {
    trap - TERM QUIT INT EXIT
    # Make sure input stream is closed. Input subprocess will do this
    # for normal terminal input but in the runtest.py case it does not
    # get a chance.
    ${PSQL} -dmal -c "SELECT io.close(0);" > /dev/null
    [ "${STDIN_PID}" ] && kill ${STDIN_PID} 2>/dev/null
}

# Load the SQL code
trap "cleanup" TERM QUIT INT EXIT
${PSQL} -tc "SELECT 1 FROM pg_database WHERE datname = 'mal'" \
    | grep -q 1 || ${PSQL} -c "CREATE DATABASE mal"
#[ "${SKIP_INIT}" ] || ${PSQL} -dmal -f $1 > /dev/null
[ "${SKIP_INIT}" ] || ${PSQL} -dmal -f $1

${PSQL} -dmal -c "SELECT io.open(0); SELECT io.open(1);" > /dev/null

# Stream from table to stdout
(
while true; do
    out="$(${PSQL} -dmal -c "SELECT io.read_or_error(1)" 2>/dev/null)" || break
    echo "${out}"
done
) &
STDOUT_PID=$!

# Perform readline input into stream table when requested
(
[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE}
while true; do
    prompt=$(${PSQL} -dmal \
        -c "SELECT io.wait_rl_prompt(0);" 2>/dev/null) || break
    IFS= read -u 0 -r -e -p "${prompt}" line || break
    if [ "${line}" ]; then
        history -s -- "${line}"        # add to history
        history -a ${RL_HISTORY_FILE}  # save history to file
    fi

    ${PSQL} -dmal -v arg="${line}" \
        -f <(echo "SELECT io.writeline(:'arg', 0);") >/dev/null || break
done
${PSQL} -dmal -c "SELECT io.close(0);" > /dev/null
) <&0 >&1 &
STDIN_PID=$!

res=0
shift
if [ $# -gt 0 ]; then
    # If there are command line arguments then run a command and exit
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    ${PSQL} -dmal -v args="(${args})" \
        -f <(echo "SELECT mal.MAIN('$(pwd)', :'args');") > /dev/null
    res=$?
else
    # Start main loop in the background
    ${PSQL} -dmal -c "SELECT mal.MAIN('$(pwd)');" > /dev/null
    res=$?
fi
wait ${STDOUT_PID}
exit ${res}

#!/bin/bash

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL_USER="${PSQL_USER:-postgres}"

PSQL="psql -q -t -A -v ON_ERROR_STOP=1 ${PSQL_USER:+-U ${PSQL_USER}}"
[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

# If mal DB is not there, force create of it
dbcheck=$(${PSQL} -c "select 1 from pg_database where datname='mal'")
[ -z "${dbcheck}" ] && SKIP_INIT=

# Load the SQL code
[ "${SKIP_INIT}" ] || ${PSQL} -f $1 >/dev/null

${PSQL} -dmal -c "SELECT stream_open(0); SELECT stream_open(1);" > /dev/null

# Stream from table to stdout
(
while true; do
    out="$(${PSQL} -dmal -c "SELECT read_or_error(1)" 2>/dev/null)" || break
    echo "${out}"
done
) &

# Perform readline input into stream table when requested
(
[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE}
while true; do
    prompt=$(${PSQL} -dmal -c "SELECT wait_rl_prompt(0);" 2>/dev/null) || break
    read -u 0 -r -e -p "${prompt}" line || break
    if [ "${line}" ]; then
        history -s -- "${line}"        # add to history
        history -a ${RL_HISTORY_FILE}  # save history to file
    fi

    ${PSQL} -dmal -v arg="${line}" \
        -f <(echo "SELECT writeline(:'arg', 0);") >/dev/null || break
done
${PSQL} -dmal -c "SELECT stream_close(0); SELECT stream_close(1);" > /dev/null
# For bizzaro reasons, removing this next statement causes repeated
# calls to fail due to DB recovery
true
) <&0 >&1 &

res=0
shift
if [ $# -gt 0 ]; then
    # If there are command line arguments then run a command and exit
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    ${PSQL} -dmal -v args="(${args})" \
        -f <(echo "SELECT RUN('$(pwd)', :'args');") > /dev/null
    res=$?
else
    # Start main loop in the background
    ${PSQL} -dmal -c "SELECT MAIN_LOOP('$(pwd)');" > /dev/null
    res=$?
fi
${PSQL} -dmal -c "SELECT stream_close(0); SELECT stream_close(1);" > /dev/null
sleep 1 # Allow DB to quiesce
exit ${res}

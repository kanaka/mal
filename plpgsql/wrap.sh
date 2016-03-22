#!/bin/bash

set -e

RL_HISTORY_FILE=${HOME}/.mal-history
SKIP_INIT="${SKIP_INIT:-}"
PSQL="psql -q -t -A -v ON_ERROR_STOP=1"

[ "${DEBUG}" ] || PSQL="${PSQL} -v VERBOSITY=terse"

# Run a command
run() {
    local func=$1 arg=$2
    if [ "${DEBUG}" ]; then
        ${PSQL} -dmal -v arg="${arg}" -f <(echo "SELECT $func(:'arg');")
    else
        ${PSQL} -dmal -v arg="${arg}" -f <(echo "SELECT $func(:'arg');") \
            2>&1 | sed 's/psql:\/dev\/fd\/[0-9]*:.: NOTICE:  //'
    fi
}

# Load the SQL code
[ "${SKIP_INIT}" ] || ${PSQL} -f $1 >/dev/null

# Set the present working directory (for slurp)
# TODO: not "multiprocess" safe.
${PSQL} -dmal -c "SELECT env_vset(0, '*PWD*', READ('$(pwd)'));" >/dev/null

shift
if [ $# -gt 0 ]; then
    args=$(for a in "$@"; do echo -n "\"$a\" "; done)
    run RUN "(${args})"
    exit 0
fi

[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE} 
while read -u 0 -r -e -p "user> " line; do
    [ -z "${line}" ] && continue
    history -s -- "${line}" # add to history
    history -a ${RL_HISTORY_FILE}  # save history to file

    # Run a command
    run REP "${line}"
done

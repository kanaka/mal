#!/bin/bash

set -e

RL_HISTORY_FILE=${HOME}/.mal-history
MODE="${MODE:-postgres}"
PSQL="psql -q -t -A"

# Load the SQL code
${PSQL} -f $1

[ -r ${RL_HISTORY_FILE} ] && history -r ${RL_HISTORY_FILE} 
while read -u 0 -r -e -p "user> " line; do
    history -s -- "${line}" # add to history
    history -a ${RL_HISTORY_FILE}  # save history to file
    # Run a command
    ${PSQL} -dmal -v line="${line}" -f <(echo "SELECT REP(:'line');")
done

#!/bin/bash

POPTS=""
while [[ ${1:0:1} = '-' ]]; do
    POPTS="${POPTS}$1 $2"
    shift; shift
done

/usr/lib/postgresql/9.4/bin/postgres \
    -c config_file=/etc/postgresql/9.4/main/postgresql.conf \
    ${POPTS} &

while ! ( echo "" > /dev/tcp/localhost/5432) 2>/dev/null; do
    echo "Waiting for postgres to start"
    sleep 1
done

if [ "${*}" ]; then
    exec "${@}"
else
    exec bash
fi

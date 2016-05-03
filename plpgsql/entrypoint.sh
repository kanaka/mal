#!/bin/bash

POSTGRES_SUDO_USER=${POSTGRES_SUDO_USER:-postgres}

POPTS=""
while [[ ${1:0:1} = '-' ]]; do
    POPTS="${POPTS}$1 $2"
    shift; shift
done

sudo --user=${POSTGRES_SUDO_USER} \
    /usr/lib/postgresql/9.4/bin/postgres \
    -c config_file=/etc/postgresql/9.4/main/postgresql.conf \
    ${POPTS} >/var/log/postgresql/output.log 2>&1 & disown -h

while ! ( echo "" > /dev/tcp/localhost/5432) 2>/dev/null; do
    echo "Waiting for postgres to start"
    sleep 1
done

if [ "${*}" ]; then
    exec "${@}"
else
    exec bash
fi

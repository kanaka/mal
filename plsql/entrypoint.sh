#!/bin/bash

echo "Starting Oracle XE"
/usr/sbin/startup.sh

if [ "${*}" ]; then
    exec "${@}"
else
    exec bash
fi

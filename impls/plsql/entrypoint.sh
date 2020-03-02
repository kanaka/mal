#!/bin/bash

case ${1} in
make*)
    echo "Skipping Oracle XE startup"
    ;;
*)
    echo "Starting Oracle XE"
    sudo /usr/sbin/startup.sh
    ;;
esac

if [ "${*}" ]; then
    exec "${@}"
else
    exec bash
fi

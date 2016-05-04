#!/bin/bash

echo "Starting Oracle XE"
sudo /usr/sbin/startup.sh

if [ "${*}" ]; then
    exec "${@}"
else
    exec bash
fi

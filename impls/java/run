#!/bin/bash
args=""
if [ "$#" -gt 0 ]; then
    args="-Dexec.args='$1'"
    for a in "${@:2}"; do
        args="$args '$a'"
    done
fi
exec mvn -quiet -e exec:java -Dexec.mainClass="mal.${STEP:-stepA_mal}" ${args:+"$args"}
